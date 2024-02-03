use std::sync::Arc;

use comemo::{Tracked, TrackedMut};
use ecow::{EcoString, EcoVec};
use typst_syntax::{ast, Source, Span};

use crate::compiler::CompileTopLevel;
use crate::diag::SourceResult;
use crate::engine::{Engine, Route};
use crate::foundations::{Label, Value};
use crate::introspection::{Introspector, Locator};
use crate::vm::{Access, CompiledClosure, DefaultValue, Pattern, Readable, Tracer};
use crate::{Library, World};

use super::{Compiler, Opcode};

/// A module that has been compiled but is not yet executed.
#[derive(Clone, Hash)]
pub struct CompiledModule {
    /// The common data.
    pub inner: Arc<Repr>,
}

impl CompiledModule {
    pub fn new(mut compiler: Compiler, span: Span, name: impl Into<EcoString>) -> Self {
        let scopes = compiler.scope.borrow();
        let exports = scopes
            .variables
            .iter()
            .map(|(name, var)| Export {
                name: name.clone(),
                value: var.register.as_readable(),
                span: var.span,
            })
            .collect();

        let jumps = compiler.remapped_instructions();
        compiler.instructions.shrink_to_fit();
        compiler.spans.shrink_to_fit();
        CompiledModule {
            inner: Arc::new(Repr {
                name: name.into(),
                span,
                registers: compiler.scope.borrow().registers.borrow().len() as usize,
                defaults: compiler.get_default_scope(),
                instructions: compiler.instructions,
                spans: compiler.spans,
                global: compiler.scope.borrow().global().clone(),
                constants: compiler.common.constants.into_values(),
                strings: compiler.common.strings.into_values(),
                closures: compiler.common.closures.into_values(),
                accesses: compiler.common.accesses.into_values(),
                labels: compiler.common.labels.into_values(),
                patterns: compiler.common.patterns.into_values(),
                isr_spans: compiler.common.spans.into_values(),
                jumps,
                joined: true,
                exports,
            }),
        }
    }
}

#[derive(Clone, Hash)]
pub struct Repr {
    /// The name of the module.
    pub name: EcoString,
    /// The span where the module was defined.
    pub span: Span,
    /// The instructions as byte code.
    pub instructions: Vec<Opcode>,
    /// The spans of the instructions.
    pub spans: Vec<Span>,
    /// The global library.
    pub global: Library,
    /// The number of registers needed for the module.
    pub registers: usize,
    /// The list of constants.
    pub constants: Vec<Value>,
    /// The list of strings.
    pub strings: Vec<Value>,
    /// The list of closures.
    pub closures: Vec<CompiledClosure>,
    /// The accesses.
    pub accesses: Vec<Access>,
    /// The list of labels.
    pub labels: Vec<Label>,
    /// The list of patterns.
    pub patterns: Vec<Pattern>,
    /// The default values of variables.
    pub defaults: EcoVec<DefaultValue>,
    /// The spans used in the module.
    pub isr_spans: Vec<Span>,
    /// The jumps used in the module.
    pub jumps: Vec<usize>,
    /// Whether this module returns a joined value.
    pub joined: bool,
    /// The exports of the module.
    pub exports: Vec<Export>,
}

#[derive(Debug, Clone, Hash)]
pub struct Export {
    /// The name of the export.
    pub name: EcoString,
    /// The value of the export.
    pub value: Readable,
    /// The span where the export was defined.
    pub span: Span,
}

#[comemo::memoize]
#[typst_macros::time(name = "module compile", span = source.root().span())]
pub fn compile_module(
    source: &Source,
    world: Tracked<dyn World + '_>,
    introspector: Tracked<Introspector>,
    route: Tracked<Route>,
    locator: Tracked<Locator>,
    tracer: TrackedMut<Tracer>,
) -> SourceResult<CompiledModule> {
    // Prevent cyclic evaluation.
    let id = source.id();
    if route.contains(id) {
        panic!("Tried to cyclicly evaluate {:?}", id.vpath());
    }

    let mut locator = Locator::chained(locator);
    let mut engine = Engine {
        world,
        introspector,
        route: Route::extend(route),
        locator: &mut locator,
        tracer,
    };

    // Parse the source.
    let root = source.root();

    // Check for well-formedness unless we are in trace mode.
    let errors = root.errors();
    if !errors.is_empty() {
        return Err(errors.into_iter().map(Into::into).collect());
    }

    // Evaluate the module.
    let markup = root.cast::<ast::Markup>().unwrap();

    // Assemble the module.
    let name = id
        .vpath()
        .as_rootless_path()
        .file_stem()
        .unwrap_or_default()
        .to_string_lossy();

    // Prepare Compiler.
    let mut compiler = Compiler::module(engine.world.library().clone().into_inner());

    // Compile the module.
    markup.compile_top_level(&mut engine, &mut compiler)?;

    Ok(CompiledModule::new(compiler, root.span(), name))
}
