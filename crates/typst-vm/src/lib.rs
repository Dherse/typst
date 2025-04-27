//! Typst's code compiler & interpreter.

pub mod closure;
mod compiler;
mod vm;

use std::sync::Arc;

pub use self::vm::Vm;
use closure::{Closure, Param};
use compiler::{CompileTopLevel, Compiler, DeferredImports, InstructionList};
use ecow::EcoString;
use typst_library::foundations::{Args, Closure as LibClosure, IntoValue};
pub use typst_library::routines::EvalMode;

use comemo::{Track, Tracked, TrackedMut};
use typst_library::diag::{bail, At, SourceResult};
use typst_library::engine::{Engine, Route, Sink, Traced};
use typst_library::foundations::{Binding, Content, Context, Func, Module, Scope, Value};
use typst_library::introspection::Introspector;
use typst_library::routines::Routines;
use typst_library::{Library, World};
use typst_syntax::{
    ast, parse, parse_code, parse_math, FileId, Source, Span, SyntaxNode,
};
use typst_utils::LazyHash;
use vm::{ControlFlow, Instructions, Readable};

/// Evaluate a source file and return the resulting module.
#[comemo::memoize]
#[typst_macros::time(name = "compile_and_run", span = source.root().span())]
pub fn eval(
    routines: &Routines,
    world: Tracked<dyn World + '_>,
    traced: Tracked<Traced>,
    mut sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    source: &Source,
) -> SourceResult<Module> {
    eprintln!("{:?}", std::mem::size_of::<Instructions>());
    Instructions::print_size();
    // Prevent cyclic evaluation.
    let id = source.id();
    if route.contains(id) {
        panic!("Tried to cyclicly evaluate {:?}", id.vpath());
    }

    // Assemble the module.
    let name = id
        .vpath()
        .as_rootless_path()
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy();

    // Compile the code
    let compiled = compile(
        routines,
        world,
        route,
        TrackedMut::reborrow_mut(&mut sink),
        source.root(),
        EvalMode::Markup,
        Some(world.library()),
        Some(name.clone().into()),
        None,
    )?;

    // compiled.pretty_print();

    // Evaluate the code
    eval_scope(
        routines,
        world,
        TrackedMut::reborrow_mut(&mut sink),
        traced,
        &compiled,
        &name,
        id,
    )
}

/// Evaluate a string as code and return the resulting value.
///
/// Everything in the output is associated with the given `span`.
#[comemo::memoize]
pub fn eval_string(
    routines: &Routines,
    world: Tracked<dyn World + '_>,
    mut sink: TrackedMut<Sink>,
    string: &str,
    span: Span,
    mode: EvalMode,
    scope: Scope,
) -> SourceResult<Value> {
    let mut root = match mode {
        EvalMode::Code => parse_code(string),
        EvalMode::Markup => parse(string),
        EvalMode::Math => parse_math(string),
    };

    root.synthesize(span);

    // Check for well-formedness.
    let errors = root.errors();
    if !errors.is_empty() {
        return Err(errors.into_iter().map(Into::into).collect());
    }

    // Compile the code
    let compiled = compile(
        routines,
        world,
        Route::default().track(),
        TrackedMut::reborrow_mut(&mut sink),
        &root,
        mode,
        Some(world.library()),
        None,
        Some(scope),
    )?;

    let traced = Traced::default();
    eval_inner(
        routines,
        world,
        TrackedMut::reborrow_mut(&mut sink),
        traced.track(),
        &compiled,
    )
}

#[typst_macros::time(name = "eval", span = compiled.span())]
fn eval_inner(
    routines: &Routines,
    world: Tracked<dyn World + '_>,
    sink: TrackedMut<Sink>,
    traced: Tracked<Traced>,
    compiled: &Arc<LazyHash<CompiledSource>>,
) -> SourceResult<Value> {
    // Prepare the engine.
    let introspector = Introspector::default();
    let engine = Engine {
        routines,
        world,
        introspector: introspector.track(),
        traced,
        sink,
        route: Route::default(),
    };

    // Prepare VM.
    let context = Context::none();
    let mut slots = compiled.slots();
    let mut vm = Vm::new(
        &**world.library(),
        compiled.instructions(),
        compiled.constants(),
        &mut slots,
        engine,
        context.track(),
    )
    .with_display(compiled.display);

    let ControlFlow::Done(value) = vm.run(None)? else {
        bail!(compiled.span, "string did not produce a value"; hint: "this is a compiler bug")
    };

    Ok(value)
}

#[typst_macros::time(name = "eval", span = compiled.span())]
fn eval_scope(
    routines: &Routines,
    world: Tracked<dyn World + '_>,
    sink: TrackedMut<Sink>,
    traced: Tracked<Traced>,
    compiled: &Arc<LazyHash<CompiledSource>>,
    name: &str,
    id: FileId,
) -> SourceResult<Module> {
    // Prepare the engine.
    let introspector = Introspector::default();
    let engine = Engine {
        routines,
        world,
        introspector: introspector.track(),
        traced,
        sink,
        route: Route::default(),
    };

    // Prepare VM.
    let context = Context::none();
    let mut slots = compiled.slots();
    let mut vm = Vm::new(
        &**world.library(),
        compiled.instructions(),
        compiled.constants(),
        &mut slots,
        engine,
        context.track(),
    )
    .with_display(compiled.display);

    let ControlFlow::Done(value) = vm.run(None)? else {
        bail!(compiled.span, "string did not produce a value"; hint: "this is a compiler bug")
    };

    // We run the VM.
    let output = value.cast::<Content>().at(compiled.span)?;

    // TODO: clean this up, shouldn't use `Scope`...
    let mut top = Scope::new();
    for (name, readable, span) in compiled.exports() {
        let value = vm.get(readable, compiled.span)?;

        top.bind(name.clone(), Binding::new(value, span));
    }

    Ok(Module::new(name, top).with_content(output).with_file_id(id))
}

/// Call the function in the context with the arguments.
#[comemo::memoize]
#[allow(clippy::too_many_arguments)]
pub fn eval_closure(
    func: &Func,
    closure: &LibClosure,
    routines: &Routines,
    world: Tracked<dyn World + '_>,
    introspector: Tracked<Introspector>,
    traced: Tracked<Traced>,
    sink: TrackedMut<Sink>,
    route: Tracked<Route>,
    context: Tracked<Context>,
    mut args: Args,
) -> SourceResult<Value> {
    // Prepare the engine.
    let engine = Engine {
        routines,
        world,
        introspector,
        traced,
        sink,
        route: Route::extend(route),
    };

    let Some(closure) = closure.downcast::<Closure>() else {
        bail!(func.span(), "expected a VM closure"; hint: "this is a compiler bug");
    };

    // Deal with arguments.
    let num_pos_args = args.to_pos().len();
    let sink_size = num_pos_args.checked_sub(closure.num_pos_params);

    let mut slots = closure.compiled.slots();
    // Write all of the captured values to the registers.
    for (target, value) in &*closure.captures {
        slots[*target] = value.clone();
    }

    // Write the self reference to the registers.
    if let Some(self_storage) = closure.compiled.local {
        slots[self_storage] = Value::Func(func.clone());
    }

    // Write all of the arguments to the registers.
    let mut sink = None;
    for arg in &closure.params {
        match arg {
            Param::Pos { name, target } => {
                slots[*target] = args.expect::<Value>(name)?;
            }
            Param::Named { name, default, target } => {
                if let Some(value) = args.named::<Value>(name)? {
                    slots[*target] = value;
                } else if let Some(default) = default {
                    slots[*target] = default.clone()
                } else {
                    unreachable!("named arguments should always have a default value");
                }
            }
            Param::Sink { span, target } => {
                sink = Some(*target);
                if let Some(target) = target {
                    let mut arguments = Args::new(*span, std::iter::empty::<Value>());

                    if let Some(sink_size) = sink_size {
                        arguments.extend(args.consume(sink_size)?);
                    }

                    slots[*target] = arguments.into_value();
                } else if let Some(sink_size) = sink_size {
                    args.consume(sink_size)?;
                }
            }
        }
    }

    if let Some(sink) = sink {
        if let Some(sink) = sink {
            let Value::Args(sink) = &mut slots[sink] else {
                bail!(closure.compiled.span(), "sink should always be an args");
            };

            sink.items.extend(args.take().items);
        } else {
            args.take();
        }
    }

    // Ensure all arguments have been used.
    args.finish()?;

    let mut vm = Vm::new(
        &**world.library(),
        closure.compiled.instructions(),
        closure.compiled.constants(),
        &mut slots,
        engine,
        context,
    )
    .with_display(closure.compiled.display);

    match vm.run(None)? {
        ControlFlow::Return(value, _) | ControlFlow::Done(value) => Ok(value),
        _ => bail!(closure.compiled.span, "closure did not return a value"),
    }
}

#[comemo::memoize]
#[typst_macros::time(span = root.span())]
pub fn compile(
    routines: &Routines,
    world: Tracked<dyn World + '_>,
    route: Tracked<Route>,
    sink: TrackedMut<Sink>,
    root: &SyntaxNode,
    mode: EvalMode,
    library: Option<&LazyHash<Library>>,
    name: Option<EcoString>,
    pre_scope: Option<Scope>,
) -> SourceResult<Arc<LazyHash<CompiledSource>>> {
    // Check for well-formedness unless we are in trace mode.
    let errors = root.errors();
    if !errors.is_empty() {
        return Err(errors.into_iter().map(Into::into).collect());
    }

    // Prepare the engine.
    let introspector = Introspector::default();
    let traced = Traced::default();
    let mut engine = Engine {
        routines,
        world,
        introspector: introspector.track(),
        traced: traced.track(),
        sink,
        route: if let Some(id) = root.span().id() {
            Route::extend(route).with_id(id)
        } else {
            Route::extend(route)
        },
    };

    // We create a scope we will use to create deferred compilation target.
    rayon::scope(move |scope| {
        let mut instructions = InstructionList::default();
        let deferred = DeferredImports::default();
        let mut compiler = Compiler::new(
            &mut engine,
            &mut instructions,
            scope,
            library.map(|v| &**v),
            deferred,
        );

        // If there is a scope, declare it.
        for (name, binding) in pre_scope.iter().flat_map(|scope| scope.iter()) {
            compiler.declare_constant(name, binding.span(), binding.read().clone());
        }

        // Compile the code.
        match mode {
            EvalMode::Code => {
                root.cast::<ast::Code>().unwrap().compile_top_level(&mut compiler)?
            }
            EvalMode::Markup => {
                root.cast::<ast::Markup>().unwrap().compile_top_level(&mut compiler)?
            }
            EvalMode::Math => {
                // root.cast::<ast::Math>().unwrap().compile(&mut compiler)?
                todo!()
            }
        };

        let compiled = compiler.finish(
            root.span(),
            name,
            matches!(mode, EvalMode::Markup | EvalMode::Math),
        )?;

        Ok(Arc::new(LazyHash::new(compiled)))
    })
}

#[derive(Clone, Hash, PartialEq, Debug)]
struct CompiledSource {
    /// The span where this code was defined.
    span: Span,
    /// The instructions.
    instructions: InstructionList,
    /// The list of constants.
    constants: Box<[Value]>,
    /// The list of constants.
    exports: Box<[(EcoString, Readable, Span)]>,
    /// The number of slots to pre-allocate.
    slots: usize,
    /// The name of the compiled code (module or closure).
    name: Option<EcoString>,
    /// Whether the output is displayed.
    display: bool,
    /// The parameters of the code (`None` for non-closures).
    params: Option<Box<[CompiledParam]>>,
    /// The captures of the code (`None` for non-closures).
    captures: Option<Box<[Capture]>>,
    /// The local slot to declare the closure (`None` for non-closures).
    local: Option<usize>,
}

impl CompiledSource {
    pub fn with_module(
        span: Span,
        instructions: InstructionList,
        exports: Box<[(EcoString, Readable, Span)]>,
        constants: Box<[Value]>,
        slots: usize,
        name: Option<EcoString>,
        display: bool,
    ) -> Self {
        Self {
            span,
            instructions,
            constants,
            exports,
            slots,
            name,
            display,
            params: None,
            captures: None,
            local: None,
        }
    }

    pub fn with_closure(
        span: Span,
        instructions: InstructionList,
        constants: Box<[Value]>,
        slots: usize,
        name: Option<EcoString>,
        display: bool,
        params: Box<[CompiledParam]>,
        captures: Box<[Capture]>,
        local: Option<usize>,
    ) -> Self {
        Self {
            span,
            instructions,
            constants,
            exports: Box::default(),
            slots,
            name,
            display,
            params: Some(params),
            captures: Some(captures),
            local,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> Option<&EcoString> {
        self.name.as_ref()
    }

    pub fn instructions(&self) -> &[Instructions] {
        self.instructions.get()
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn slots(&self) -> Vec<Value> {
        vec![Value::None; self.slots]
    }

    pub fn exports(&self) -> impl Iterator<Item = (&EcoString, Readable, Span)> + '_ {
        self.exports.iter().map(|(key, index, span)| (key, *index, *span))
    }

    pub fn params(&self) -> impl Iterator<Item = &CompiledParam> {
        self.params.iter().flatten()
    }

    pub fn captures(&self) -> impl Iterator<Item = &Capture> {
        self.captures.iter().flatten()
    }
}

/// Evaluate an expression.
pub trait Eval {
    /// The output of evaluating the expression.
    type Output;

    /// Evaluate the expression to the output value.
    fn eval(self, vm: &mut Vm) -> SourceResult<Self::Output>;
}

#[derive(Clone, Hash, PartialEq, Debug)]
pub enum CompiledParam {
    /// A positional parameter.
    Pos(usize, EcoString),
    /// A named parameter.
    Named {
        /// The span of the parameter.
        span: Span,
        /// The location where the parameter will be stored.
        target: usize,
        /// The name of the parameter.
        name: EcoString,
        /// The default value of the parameter.
        default: Option<Readable>,
    },
    /// A sink parameter.
    Sink(Span, Option<usize>, EcoString),
}

impl CompiledParam {
    pub fn default(&self) -> Option<Readable> {
        match self {
            Self::Pos(_, _) | Self::Sink(_, _, _) => None,
            Self::Named { default, .. } => *default,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct Capture {
    /// The name of the value to capture.
    pub name: EcoString,
    /// The value of the capture **in the parent scope**.
    pub readable: Readable,
    /// Where the value is stored **in the closure's scope**.
    pub slot: usize,
    /// The span where the capture was occurs.
    pub span: Span,
}
