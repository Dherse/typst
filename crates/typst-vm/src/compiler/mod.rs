mod call;
mod code;
mod flow;
mod func;
mod import;
mod include;
mod locals;
mod markup;
mod math;
mod pattern;
mod rules;

pub use self::import::{load_deferred_module, Resolve};
pub use self::locals::Local;
use self::locals::{ConstValue, Locals};

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Deref;
use std::rc::Rc;
use std::{mem, usize};

use ecow::{eco_vec, EcoString};
use import::resolve_path;
use indexmap::IndexMap;
use typst_library::diag::{error, At, SourceDiagnostic, SourceResult};
use typst_library::engine::{Engine, Sink};
use typst_library::foundations::{Module, Value};
use typst_library::Library;
use typst_syntax::package::PackageSpec;
use typst_syntax::{FileId, Span};
use typst_utils::{hash128, ScopedDeferred};

use crate::vm::{FlowOp, Instructions, Noop, Readable};
use crate::{CompiledParam, CompiledSource};

pub trait Compile {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable>;
}

pub trait CompileTopLevel {
    fn compile_top_level(self, compiler: &mut Compiler) -> SourceResult<()>;
}

pub struct Compiler<'a, 'b> {
    /// The instructionsR
    instructions: &'a mut InstructionList,

    /// The current start index of the current instruction scope.
    scope_start: usize,

    /// Whether we are currently in a loop.
    in_loop: bool,

    /// Whether we are currently compiling in math mode.
    in_math: bool,

    /// Whether we are currently in a function/closure.
    in_function: bool,

    /// The current scope depth.
    depth: usize,

    /// The list of locals and hierarchy.
    locals: Rc<RefCell<Locals>>,

    /// the engine
    engine: &'a mut Engine<'b>,

    /// A scope for creating threads.
    scope: &'a rayon::Scope<'b>,

    /// The list of deferred module compilations.
    deferred: DeferredImports<'b>,
}

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn new(
        engine: &'a mut Engine<'b>,
        instructions: &'a mut InstructionList,
        scope: &'a rayon::Scope<'b>,
        library: Option<&Library>,
        deferred: DeferredImports<'b>,
    ) -> Self {
        Self {
            engine,
            instructions,
            scope_start: 0,
            depth: 0,
            in_loop: false,
            in_function: false,
            locals: Rc::new(RefCell::new(Locals::new(library))),
            scope,
            deferred,
            in_math: false,
        }
    }

    pub fn new_closure(
        engine: &'a mut Engine<'b>,
        locals: &Rc<RefCell<Locals>>,
        deferred: &'a DeferredImports<'b>,
        scope: &'a rayon::Scope<'b>,
        instructions: &'a mut InstructionList,
    ) -> Self {
        let locals = Locals::enter(Rc::clone(locals), true);
        Self {
            engine,
            instructions,
            scope_start: 0,
            depth: 0,
            in_loop: false,
            in_function: true,
            locals,
            scope,
            deferred: deferred.clone(),
            in_math: false,
        }
    }

    pub fn engine(&mut self) -> &mut Engine<'b> {
        &mut *self.engine
    }

    pub fn locals(&self) -> &Rc<RefCell<Locals>> {
        &self.locals
    }

    pub fn in_loop(&self) -> bool {
        self.in_loop
    }

    pub fn in_function(&self) -> bool {
        self.in_function
    }

    pub fn resolve(&self, readable: Readable) -> Option<Value> {
        match readable {
            Readable::Stack | Readable::Slot(_) | Readable::Take(_) => None,
            Readable::None | Readable::Empty => Some(Value::None),
            Readable::Auto => Some(Value::Auto),
            Readable::Const(cst) => {
                let cst = self.locals.borrow().constant(cst)?;
                match cst {
                    ConstValue::Inline(value) => Some(value),
                    ConstValue::Deferred(deferred_index, deferred_entry) => {
                        let guard = self.deferred.get(deferred_index);
                        let deferred = guard.wait().0.as_ref().ok()?;

                        match deferred_entry {
                            locals::DeferredEntry::Module => Some(deferred.clone()),
                            locals::DeferredEntry::Content(_) => {
                                if let Value::Module(module) = deferred {
                                    Some(Value::Content(module.clone().content()))
                                } else {
                                    None
                                }
                            }
                            // Any error generated in this path, will be re-generated in the `finish` method.
                            locals::DeferredEntry::Nested(_, path) => {
                                let mut path = path.iter().peekable();
                                let mut scope = deferred.scope()?;
                                while let Some((_, component)) = &path.next() {
                                    let Some(binding) = scope.get(component) else {
                                        return None;
                                    };

                                    if path.peek().is_some() {
                                        let value = binding.read();
                                        let Some(submodule) = value.scope() else {
                                            return None;
                                        };

                                        // Walk into the submodule.
                                        scope = submodule;
                                    } else {
                                        return Some(binding.read().clone());
                                    }
                                }

                                None
                            }
                        }
                    }
                }
            }
            Readable::Bool(bool_) => Some(Value::Bool(bool_)),
            Readable::Int(int) => Some(Value::Int(int)),
            Readable::Float(scalar) => Some(Value::Float(scalar.get())),
            Readable::Std(index) => {
                let locals = self.locals().borrow();
                let lib = locals.library()?;

                Some(lib.global.scope().get_by_index(index)?.read().clone())
            }
            Readable::Math(index) => {
                let locals = self.locals().borrow();
                let lib = locals.library()?;

                Some(lib.math.scope().get_by_index(index)?.read().clone())
            }
        }
    }

    /// Adds a dummy instruction and return its pointer.
    ///
    /// If the instruction is not later set, leads to a compiler error
    /// when finalizing the pointer.
    pub fn insert_pointer(&mut self) -> Pointer {
        let i = self.instructions.len();
        self.push(Noop);
        Pointer(i)
    }

    pub fn flow(&mut self) {
        if self
            .instructions
            .last()
            .map_or(false, |(depth, v)| depth != self.depth || !v.is_flow())
        {
            self.push(FlowOp);
        }
    }

    pub fn push(&mut self, insr: impl Into<Instructions>) {
        self.instructions.push(insr, self.depth);
    }

    pub fn here(&self) -> Pointer {
        Pointer(self.instructions.len())
    }

    pub fn declare(&mut self, name: &EcoString, span: Span) -> usize {
        self.locals.borrow_mut().declare(name.clone(), span)
    }

    pub fn get(&mut self, name: &EcoString, span: Span) -> SourceResult<Readable> {
        self.get_or_declare(name, span, false)
    }

    pub fn get_math(&mut self, name: &EcoString, span: Span) -> SourceResult<Readable> {
        self.locals
            .borrow_mut()
            .get(name, span, self.here(), false, self.in_math)
            .map(Readable::from)
            .ok_or_else(|| ecow::eco_vec![missing_var(span, name)])
    }

    pub fn get_name(&mut self, slot: usize) -> Option<EcoString> {
        self.locals.borrow().get_name(slot)
    }

    pub fn get_or_declare(
        &mut self,
        name: &EcoString,
        span: Span,
        declare: bool,
    ) -> SourceResult<Readable> {
        self.locals
            .borrow_mut()
            .get(name, span, self.here(), declare, self.in_math)
            .map(Readable::from)
            .ok_or_else(|| ecow::eco_vec![missing_var(span, name)])
    }

    pub fn declare_constant(
        &mut self,
        name: &EcoString,
        span: Span,
        value: impl Into<ConstValue>,
    ) -> Readable {
        let index =
            self.locals
                .borrow_mut()
                .declare_const(name.clone(), span, value.into());
        Readable::Const(index)
    }

    pub fn constant(&mut self, value: impl Into<ConstValue>) -> Readable {
        let index = self.locals.borrow_mut().declare_anonymous_const(value.into());
        Readable::Const(index)
    }

    pub fn write<I: Into<Instructions>>(&mut self, pointer: Pointer, instruction: I) {
        self.instructions.1[pointer.as_raw()] = instruction.into();
    }

    pub fn enter(
        &mut self,
        fn_: impl FnOnce(&mut Self) -> SourceResult<()>,
    ) -> SourceResult<PointerRange> {
        let old_start = self.scope_start;
        let start = self.instructions.len();

        self.scope_start = start;
        self.depth += 1;
        fn_(self)?;
        self.depth -= 1;
        self.scope_start = old_start;

        Ok(PointerRange(start, self.instructions.len()))
    }

    pub fn scope(
        &mut self,
        looping: bool,
        fn_: impl FnOnce(&mut Self) -> SourceResult<()>,
    ) -> SourceResult<PointerRange> {
        let old_start = self.scope_start;
        let start = self.instructions.len();
        let mut in_loop = looping || self.in_loop();

        // Add a new level to the local variable scope.
        let mut scope = Locals::enter(self.locals.clone(), false);
        std::mem::swap(&mut self.locals, &mut scope);
        std::mem::swap(&mut self.in_loop, &mut in_loop);
        self.depth += 1;

        self.scope_start = start;
        fn_(self)?;
        self.scope_start = old_start;

        // Reset the scope.
        std::mem::swap(&mut self.locals, &mut scope);
        std::mem::swap(&mut self.in_loop, &mut in_loop);
        self.depth -= 1;

        Ok(PointerRange(start, self.instructions.len()))
    }

    pub fn resolve_constants(&mut self) -> SourceResult<Box<[Value]>> {
        let waiters = self.deferred.imports.borrow();
        let mut modules = vec![None; waiters.len()];
        let mut module = |index: usize| -> SourceResult<&Value> {
            if let Some(module) = &modules[index] {
                return Ok(*module);
            }

            let (module, sink) = waiters[index].wait();
            let module = module.as_ref().map_err(|e| e.clone())?;

            if !sink.is_empty() {
                let mut sink = sink.clone();
                self.engine
                    .sink
                    .extend(sink.delayed(), sink.warnings(), sink.values());
            }

            modules[index] = Some(module);

            Ok(module)
        };

        let consts = mem::take(&mut *self.locals.borrow_mut().constants.borrow_mut());

        let mut constants = Vec::with_capacity(consts.values.len());
        let mut errors = eco_vec![];
        for value in consts.values {
            match value {
                ConstValue::Inline(value) => constants.push(value),
                ConstValue::Deferred(deferred, deferred_entry) => {
                    let value = module(deferred)?;
                    match deferred_entry {
                        locals::DeferredEntry::Module => constants.push(value.clone()),
                        locals::DeferredEntry::Content(span) => {
                            let module = value.clone().cast::<Module>().at(span)?;
                            constants.push(Value::Content(module.content()))
                        }
                        locals::DeferredEntry::Nested(span, path) => {
                            match resolve_path(
                                value,
                                span,
                                path.iter().map(|(span, name)| (*span, &**name)),
                            ) {
                                Ok(value) => constants.push(value.clone()),
                                Err(e) => errors.push(e),
                            };
                        }
                    }
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(constants.into_boxed_slice())
    }

    pub fn finish(
        mut self,
        span: Span,
        name: Option<EcoString>,
        display: bool,
    ) -> SourceResult<CompiledSource> {
        debug_assert!(Rc::strong_count(self.locals()) == 1);

        let exports = self.locals().borrow().variables().collect::<Box<[_]>>();
        let constants = self.resolve_constants()?;
        Ok(CompiledSource::with_module(
            span,
            mem::take(self.instructions),
            exports,
            constants,
            self.locals().borrow().len(),
            name,
            display,
        ))
    }

    pub fn finish_closure(
        mut self,
        span: Span,
        name: Option<EcoString>,
        params: Vec<CompiledParam>,
        local: Option<usize>,
        display: bool,
    ) -> SourceResult<CompiledSource> {
        let captures = self
            .locals()
            .borrow_mut()
            .captured_variables()
            .map(|(name, slot, parent_slot, span)| crate::Capture {
                name: name.clone(),
                readable: Readable::Slot(parent_slot),
                slot,
                span,
            })
            .collect::<Vec<_>>();

        let constants = self.resolve_constants()?;
        Ok(CompiledSource::with_closure(
            span,
            mem::take(self.instructions),
            constants,
            self.locals().borrow().len(),
            name,
            display,
            params.into_boxed_slice(),
            captures.into_boxed_slice(),
            local,
        ))
    }
}

#[must_use]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pointer(usize);

impl Pointer {
    pub const fn from_raw(ptr: usize) -> Self {
        Self(ptr)
    }

    pub const fn detached() -> Self {
        Self(usize::MAX)
    }

    pub fn as_raw(&self) -> usize {
        self.0
    }

    pub fn is_detached(&self) -> bool {
        self == &Self::detached()
    }
}

#[must_use]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PointerRange(usize, usize);

impl PointerRange {
    pub fn start(&self) -> Pointer {
        Pointer(self.0)
    }

    pub fn end(&self) -> Pointer {
        Pointer(self.1)
    }

    pub fn len(&self) -> usize {
        self.1 - self.0
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Debug, Clone, Default, Hash, PartialEq)]
pub struct InstructionList(Vec<usize>, Vec<Instructions>);

impl InstructionList {
    pub fn push(&mut self, insr: impl Into<Instructions>, depth: usize) {
        self.0.push(depth);
        self.1.push(insr.into())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self) -> &[Instructions] {
        &self.1
    }

    fn last(&self) -> Option<(usize, &Instructions)> {
        debug_assert!(self.0.len() == self.1.len());

        self.1.last().map(|last| (*self.0.last().unwrap(), last))
    }
}

#[derive(Default)]
pub struct Constants {
    values: Vec<ConstValue>,
    mapper: HashMap<u128, usize>,
}

impl Constants {
    pub fn insert(&mut self, constant: ConstValue) -> usize {
        let hash = hash128(&constant);
        *self.mapper.entry(hash).or_insert_with(|| {
            self.values.push(constant);
            self.values.len() - 1
        })
    }

    pub fn get(&self, constant: usize) -> Option<&ConstValue> {
        self.values.get(constant)
    }
}

#[cold]
fn missing_var(span: Span, name: &str) -> SourceDiagnostic {
    error!(span, "unknown variable: {name}")
}

type ImportDeferred<'scope> = ScopedDeferred<'scope, (SourceResult<Value>, Sink)>;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ImportKey {
    FileId(FileId),
    Package(PackageSpec),
}

impl From<FileId> for ImportKey {
    fn from(value: FileId) -> Self {
        Self::FileId(value)
    }
}

impl From<PackageSpec> for ImportKey {
    fn from(value: PackageSpec) -> Self {
        Self::Package(value)
    }
}

#[derive(Clone, Default)]
pub struct DeferredImports<'scope> {
    imports: Rc<RefCell<IndexMap<ImportKey, ImportDeferred<'scope>>>>,
}
impl<'scope> DeferredImports<'scope> {
    pub fn get(&self, id: usize) -> DeferredGuard<'_, 'scope> {
        DeferredGuard(self.imports.borrow(), id)
    }

    pub fn wait(&self, id: usize) -> (SourceResult<Value>, Sink) {
        self.imports.borrow()[id].wait().clone()
    }

    pub fn insert_if_missing(
        &self,
        value: impl Into<ImportKey>,
        scope: &rayon::Scope<'scope>,
        f: impl FnOnce() -> (SourceResult<Value>, Sink) + Send + Sync + 'scope,
    ) -> SourceResult<usize> {
        let key = value.into();
        let mut this = self.imports.borrow_mut();
        if let Some(old) = this.get_index_of(&key) {
            Ok(old)
        } else {
            let index = this.len();
            this.insert(key, ImportDeferred::new(scope, f));
            Ok(index)
        }
    }
}

pub struct DeferredGuard<'a, 'scope>(
    std::cell::Ref<'a, IndexMap<ImportKey, ImportDeferred<'scope>>>,
    usize,
);

impl<'a, 'scope> Deref for DeferredGuard<'a, 'scope> {
    type Target = ImportDeferred<'scope>;

    fn deref(&self) -> &Self::Target {
        &self.0[self.1]
    }
}
