mod access;
mod binding;
mod call;
mod closure;
mod code;
mod flow;
mod import;
mod include;
mod markup;
mod math;
mod ops;
mod pattern;
mod registers;
mod remapper;
mod rules;
mod scope;

use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;

use ecow::EcoString;
use typst_syntax::Span;

use crate::diag::SourceResult;
use crate::engine::Engine;
use crate::foundations::{IntoValue, Label, Value};
use crate::lang::compiled::CodeCapture;
use crate::Library;

use super::compiled::{
    CompiledClosure, CompiledCode, CompiledParam, DefaultValue, Export,
};
use super::opcodes::Opcode;
use super::operands::{
    AccessId, ClosureId, Constant, LabelId, ModuleId, PatternId, Pointer, Readable,
    SpanId, Writable,
};

pub use self::access::*;
pub use self::import::*;
pub use self::pattern::*;
pub use self::registers::*;
pub use self::remapper::*;
pub use self::scope::*;

pub use crate::__copy_constant as copy_constant;

const DEFAULT_CAPACITY: usize = 8 << 10;

#[macro_export]
#[doc(hidden)]
macro_rules! __copy_constant {
    ($this:expr, $compiler:expr, $engine:expr, $output:expr) => {
        let constant = $this.compile_to_readable($compiler, $engine)?;

        // Copy the constant into the output.
        $compiler.copy($this.span(), constant, $output);
    };
}

pub struct Compiler<'lib> {
    /// The name of the current function (if any).
    pub name: Option<EcoString>,
    /// The list of instructions.
    instructions: Vec<Opcode>,
    /// The list of spans for each instruction.
    isr_spans: Vec<Span>,
    /// The current scope.
    pub(super) scope: Rc<RefCell<Scope<'lib>>>,
    /// The constant remapper.
    constants: Remapper<Constant, Value>,
    /// The label remapper.
    labels: Remapper<LabelId, Label, SimpleRemapper<LabelId, Label>>,
    /// The span remapper.
    spans: Remapper<SpanId, Span, SimpleRemapper<SpanId, Span>>,
    /// The access remapper.
    accesses: Remapper<AccessId, Access, SimpleRemapper<AccessId, Access>>,
    /// The pattern remapper.
    patterns: Remapper<PatternId, Pattern, SimpleRemapper<PatternId, Pattern>>,
    /// Dynamic module remapper.
    modules: Remapper<ModuleId, DynamicModule, SimpleRemapper<ModuleId, DynamicModule>>,
    /// The closure remapper.
    closures: Remapper<ClosureId, CompiledClosure>,
    /// Whether we are in fact in a contextual rather than a function scope.
    contextual: bool,
    /// The current scope counter.
    scopes: u16,
    /// The current jump counter.
    jumps: u16,
    /// Whether a value in the current scope is being observed.
    observed: bool,
}

impl<'lib> Compiler<'lib> {
    /// Creates a new compiler for a closure from a parent compiler.
    pub fn new_closure(parent: &Self, name: Option<EcoString>) -> Self {
        Self {
            name,
            instructions: Vec::with_capacity(DEFAULT_CAPACITY),
            isr_spans: Vec::with_capacity(DEFAULT_CAPACITY),
            scope: Rc::new(RefCell::new(Scope::new(
                true,
                false,
                Some(parent.library()),
                None,
                Some(parent.scope.clone()),
                Some(RegisterAllocator::new()),
            ))),
            constants: Remapper::default(),
            labels: Remapper::default(),
            spans: Remapper::default(),
            accesses: Remapper::default(),
            patterns: Remapper::default(),
            modules: Remapper::default(),
            closures: Remapper::default(),
            scopes: 0,
            jumps: 0,
            contextual: false,
            observed: false,
        }
    }

    /// Creates a new compiler for a module.
    pub fn new_module(library: &'lib Library) -> Self {
        Self {
            name: None,
            instructions: Vec::with_capacity(DEFAULT_CAPACITY),
            isr_spans: Vec::with_capacity(DEFAULT_CAPACITY),
            scope: Rc::new(RefCell::new(Scope::new(
                false,
                false,
                Some(library),
                None,
                None,
                Some(RegisterAllocator::new()),
            ))),
            constants: Remapper::default(),
            labels: Remapper::default(),
            spans: Remapper::default(),
            accesses: Remapper::default(),
            patterns: Remapper::default(),
            modules: Remapper::default(),
            closures: Remapper::default(),
            scopes: 0,
            jumps: 0,
            contextual: false,
            observed: false,
        }
    }

    /// Set whether we are compiling a `contextual` scope.
    pub fn with_contextual(mut self, contextual: bool) -> Self {
        self.contextual = contextual;
        self
    }

    /// Appends an instruction and its span to the instruction list.
    pub fn insr(&mut self, span: Span, opcode: Opcode) {
        debug_assert!(self.instructions.len() == self.isr_spans.len());

        self.instructions.push(opcode);
        self.isr_spans.push(span);
    }

    /// Whether we are in a function.
    pub fn in_function(&self) -> bool {
        self.scope.borrow().in_function()
    }

    /// Whether we are in a loop.
    pub fn in_loop(&self) -> bool {
        self.scope.borrow().in_loop()
    }

    /// Get the global library.
    pub fn library(&self) -> &'lib Library {
        self.scope.borrow().global().expect("failed to get library")
    }

    /// Allocates a register.
    pub fn allocate(&self) -> RegisterGuard {
        self.scope.borrow().allocate()
    }

    /// Allocates a pristine register.
    pub fn allocate_pristine(&self) -> PristineRegisterGuard {
        self.scope.borrow().allocate_pristine()
    }

    /// Declares a new variable.
    pub fn declare(&self, span: Span, name: &str) -> RegisterGuard {
        self.scope.borrow_mut().declare(span, name, None)
    }

    /// Declares a new variable in a specific register.
    pub fn declare_to_register(
        &self,
        span: Span,
        name: &str,
        output: impl Into<RegisterGuard>,
    ) {
        self.scope.borrow_mut().declare_to_register(span, name, output.into())
    }

    /// Declares a new variable.
    pub fn declare_default(
        &self,
        span: Span,
        name: &str,
        default: impl IntoValue,
    ) -> RegisterGuard {
        self.scope
            .borrow_mut()
            .declare(span, name, Some(default.into_value()))
    }

    /// Creates a new jump marker.
    pub fn marker(&mut self) -> Pointer {
        let id = self.jumps;
        self.jumps += 1;
        Pointer::new(id)
    }

    /// Creates a new flow marker.
    pub fn flow(&mut self) {
        self.insr(Span::detached(), Opcode::Flow);
    }

    /// Inserts a new constant into the constant pool.
    pub fn const_(&mut self, value: impl IntoValue) -> Constant {
        self.constants.insert(value.into_value())
    }

    /// Get a constant by its ID.
    pub fn get_constant(&self, constant: &Constant) -> Option<&Value> {
        self.constants.get(constant)
    }

    /// Inserts a new label into the label pool.
    pub fn label(&mut self, label: Label) -> LabelId {
        self.labels.insert(label)
    }

    /// Inserts a new span into the span pool.
    pub fn span(&mut self, span: Span) -> SpanId {
        self.spans.insert(span)
    }

    /// Inserts a new access into the access pool.
    pub fn access(&mut self, access: Access) -> AccessId {
        self.accesses.insert(access)
    }

    /// Get an access by its ID.
    pub fn get_access(&self, access: &AccessId) -> Option<&Access> {
        self.accesses.get(access)
    }

    /// Inserts a new pattern into the pattern pool.
    pub fn pattern(&mut self, pattern: Pattern) -> PatternId {
        self.patterns.insert(pattern)
    }

    /// Inserts a new dynamic module into the module pool.
    pub fn module(&mut self, module: DynamicModule) -> ModuleId {
        self.modules.insert(module)
    }

    /// Inserts a new closure into the closure pool.
    pub fn closure(&mut self, closure: CompiledClosure) -> ClosureId {
        self.closures.insert(closure)
    }

    pub fn observe(&mut self, span: Span, value: impl Into<Readable>) {
        self.observed = true;
        self.observe_isr(span, value);
    }

    /// Read a variable.
    pub fn read(
        &mut self,
        span: Span,
        name: &str,
        mutable: bool,
    ) -> Option<ReadableGuard> {
        if mutable {
            // We ignore the error as this is only meant to ensure
            // modified vars aren't declared as const.
            let _ = self.scope.borrow_mut().write(name);
        }

        self.scope.borrow_mut().read(span, name)
    }

    /// Read a math variable.
    pub fn read_math(&mut self, span: Span, name: &str) -> Option<ReadableGuard> {
        self.scope.borrow_mut().read_math(span, name)
    }

    /// Tries to resolve a register to a variable (if any)
    pub fn resolve_var(&self, register: &RegisterGuard) -> Option<Variable> {
        self.scope.borrow().resolve_var(register)
    }

    /// Copy a value into a variable.
    pub fn mutate_variable(&self, variable: &str) {
        self.scope.borrow_mut().write(variable).ok();
    }

    /// Tries to resolve a register to a default value (if any)
    pub fn resolve_default(&self, register: &RegisterGuard) -> Option<Value> {
        self.scope.borrow().resolve_default(register)
    }

    /// Tries and resolve any readable
    pub fn resolve(&self, readable: impl Into<Readable>) -> Option<Cow<'_, Value>> {
        let readable = readable.into();
        match readable {
            Readable::Const(cst) => self.get_constant(&cst).map(Cow::Borrowed),
            Readable::Global(glob) => {
                let glob = self.library().global.field_by_index(glob.as_raw() as usize)?;
                Some(Cow::Borrowed(glob))
            }
            Readable::Math(math) => {
                let math = self.library().math.field_by_index(math.as_raw() as usize)?;
                Some(Cow::Borrowed(math))
            }
            Readable::Label(label) => {
                let label = self.labels.get(&label)?;
                Some(Cow::Owned(Value::Label(*label)))
            }
            Readable::Reg(_) => None,
            Readable::Bool(val) => {
                if val {
                    Some(Cow::Borrowed(&Value::Bool(true)))
                } else {
                    Some(Cow::Borrowed(&Value::Bool(false)))
                }
            }
            Readable::GlobalModule => Some(Cow::Borrowed(&self.library().std)),
            Readable::None => Some(Cow::Borrowed(&Value::None)),
            Readable::Auto => Some(Cow::Borrowed(&Value::Auto)),
        }
    }

    /// Enter a new unspecified scope (i.e `{}`, body of if-else, etc.).
    #[typst_macros::time(name = "enter scope", span = span)]
    pub fn enter(
        &mut self,
        engine: &mut Engine,
        span: Span,
        output: impl Into<Writable>,
        f: impl FnOnce(&mut Self, &mut Engine) -> SourceResult<bool>,
    ) -> SourceResult<()> {
        self.enter_generic(engine, false, f, |compiler, _, len, is_content| {
            compiler.enter_isr(span, len as u32, is_content, output);
            Ok(())
        })
    }

    /// Enter any other kind of scope (i.e loop bodies).
    pub fn enter_generic<O>(
        &mut self,
        engine: &mut Engine,
        looping: bool,
        f: impl FnOnce(&mut Self, &mut Engine) -> SourceResult<O>,
        pre: impl FnOnce(&mut Self, &mut Engine, usize, O) -> SourceResult<()>,
    ) -> SourceResult<()> {
        // The new sub-scope.
        let mut scope = Rc::new(RefCell::new(Scope::new(
            false,
            looping,
            None,
            Some(self.scope.clone()),
            None,
            None,
        )));

        // The temporary instructions buffer
        let mut instructions = Vec::with_capacity(DEFAULT_CAPACITY);
        let mut isr_spans = Vec::with_capacity(DEFAULT_CAPACITY);

        self.scopes += 1;

        std::mem::swap(&mut self.scope, &mut scope);
        std::mem::swap(&mut self.instructions, &mut instructions);
        std::mem::swap(&mut self.isr_spans, &mut isr_spans);

        let out = f(self, engine)?;

        std::mem::swap(&mut self.scope, &mut scope);
        std::mem::swap(&mut self.instructions, &mut instructions);
        std::mem::swap(&mut self.isr_spans, &mut isr_spans);

        let len = instructions.len();
        pre(self, engine, len, out)?;

        self.isr_spans.extend(isr_spans);
        self.instructions.extend(instructions);
        Ok(())
    }

    #[typst_macros::time(name = "finish closure", span = span)]
    pub fn finish_closure(
        mut self,
        span: Span,
        params: Vec<CompiledParam>,
        local: Option<RegisterGuard>,
        display: bool,
    ) -> SourceResult<CompiledCode> {
        let mut scopes = self.scope.borrow_mut();

        // Convert closure captures to compiled format.
        let captures: Vec<_> = scopes
            .captures
            .take()
            .unwrap_or_default()
            .values()
            .map(|capture| CodeCapture {
                name: capture.name,
                span: capture.span,
                readable: capture.readable.clone().into(),
                register: capture.register.clone().into(),
            })
            .collect();

        drop(scopes);

        // Re-borrow immutably.
        let scopes = self.scope.borrow();

        // Remap jump instructions.
        let mut jumps = self.remap_jumps();
        jumps.shrink_to_fit();

        // Shrink instructions.
        self.instructions.shrink_to_fit();
        self.isr_spans.shrink_to_fit();

        let registers = scopes.registers.as_ref().map_or(0, |r| r.len());
        Ok(CompiledCode {
            display,
            defaults: self.get_default_scope().into(),
            name: self.name,
            observed: self.observed,
            span,
            instructions: self.instructions.into(),
            spans: self.isr_spans.into(),
            global: scopes.global().expect("failed to get library").clone(),
            registers,
            constants: self.constants.into_values().into(),
            closures: self.closures.into_values().into(),
            accesses: self.accesses.into_values().into(),
            labels: self.labels.into_values().into(),
            patterns: self.patterns.into_values().into(),
            isr_spans: self.spans.into_values().into(),
            modules: self.modules.into_values().into(),
            jumps: jumps.into(),
            exports: None,
            captures: Some(captures.into()),
            params: Some(params.into()),
            self_storage: local.map(|r| r.as_register()),
        })
    }

    #[typst_macros::time(name = "finish module", span = span)]
    pub fn finish_module(
        mut self,
        span: Span,
        name: impl Into<EcoString>,
        mut exports: Vec<Export>,
        display: bool,
    ) -> CompiledCode {
        // Get the global library.
        let global = self.library().clone();

        let scopes = self.scope.borrow();
        debug_assert!(scopes.captures.is_none());

        // Remap jump instructions.
        let mut jumps = self.remap_jumps();
        jumps.shrink_to_fit();

        // Shrink instructions.
        self.instructions.shrink_to_fit();
        self.isr_spans.shrink_to_fit();
        exports.shrink_to_fit();

        let registers = scopes.registers.as_ref().map_or(0, |r| r.len());
        CompiledCode {
            display,
            defaults: self.get_default_scope().into(),
            span,
            registers,
            observed: self.observed,
            name: Some(name.into()),
            instructions: self.instructions.into(),
            spans: self.isr_spans.into(),
            global,
            constants: self.constants.into_values().into(),
            closures: self.closures.into_values().into(),
            accesses: self.accesses.into_values().into(),
            labels: self.labels.into_values().into(),
            patterns: self.patterns.into_values().into(),
            isr_spans: self.spans.into_values().into(),
            modules: self.modules.into_values().into(),
            jumps: jumps.into(),
            captures: None,
            params: None,
            self_storage: None,
            exports: Some(exports.into()),
        }
    }

    /// Remaps jump instructions such that they're relative to the scope (if needed).
    /// This creates a table for each jump instruction to the instruction it jumps to.
    fn remap_jumps(&self) -> Vec<usize> {
        // If there are no jumps, we can return early.
        if self.jumps == 0 {
            return Vec::new();
        }

        let mut iter = self.instructions.iter();
        let mut jumps = vec![usize::MAX; self.jumps as usize];

        fn remap(
            iter: &mut dyn Iterator<Item = &Opcode>,
            jumps: &mut Vec<usize>,
            count: &mut usize,
        ) {
            let mut i = 0;
            while let Some(next) = iter.next() {
                match next {
                    Opcode::PointerMarker(id) => {
                        jumps[id.marker.as_raw() as usize] = i;
                        *count -= 1;
                    }
                    Opcode::Enter(enter) => {
                        remap(&mut iter.take(enter.len as usize), jumps, count);
                        i += enter.len as usize;
                    }
                    Opcode::While(while_) => {
                        remap(&mut iter.take(while_.len as usize), jumps, count);
                        i += while_.len as usize;
                    }
                    Opcode::Iter(iter_op) => {
                        remap(&mut iter.take(iter_op.len as usize), jumps, count);
                        i += iter_op.len as usize;
                    }
                    _ => {}
                }

                i += 1;
                if *count == 0 {
                    break;
                }
            }
        }

        if self.jumps > 0 {
            let mut i = self.jumps as usize;
            remap(&mut iter, &mut jumps, &mut i);
        }

        jumps
    }

    fn get_default_scope(&self) -> Vec<DefaultValue> {
        self.scope
            .borrow()
            .defaults
            .iter()
            .flatten()
            .map(|(target, value)| DefaultValue {
                target: target.as_register(),
                value: value.clone(),
            })
            .collect()
    }
}

pub trait CompileTopLevel {
    /// Compile the current AST node as the top-level node.
    /// This assumes that the output is always the joiner.
    fn compile_top_level(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<()>;
}

trait Compile {
    /// Compile the current AST node.
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()>;

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<ReadableGuard> {
        let output = compiler.allocate();
        self.compile(compiler, engine, output.clone().into())
            .map(|_| output.into())
    }
}
