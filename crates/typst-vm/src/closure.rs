use std::hash::Hash;
use std::sync::Arc;

use ecow::EcoString;
use typst_library::diag::{bail, SourceResult};
use typst_library::foundations::{ClosureInner, Value};
use typst_macros::cast;
use typst_syntax::Span;
use typst_utils::LazyHash;

use crate::{CompiledParam, CompiledSource};

use super::compiler::Compiler;

/// A closure that has been instantiated.
#[derive(Clone, Hash, PartialEq)]
pub struct Closure {
    /// The compiled code of the closure.
    pub(crate) compiled: Arc<LazyHash<CompiledSource>>,
    /// The parameters of the closure.
    pub(crate) params: Vec<Param>,
    /// The captured values and where to store them.
    pub(crate) captures: Vec<(usize, Value)>,
    /// The number of positional parameters.
    pub(crate) num_pos_params: usize,
}

impl Closure {
    /// Creates a new closure.
    pub(crate) fn new(
        compiled: Arc<LazyHash<CompiledSource>>,
        params: Vec<Param>,
        captures: Vec<(usize, Value)>,
    ) -> Closure {
        let num_pos_params =
            params.iter().filter(|p| matches!(p, Param::Pos { .. })).count();

        Self { compiled, params, captures, num_pos_params }
    }

    /// Get the name of the closure.
    pub fn name(&self) -> Option<&str> {
        self.compiled.name().map(|v| &**v)
    }

    pub(crate) fn no_instance(
        compiled: Arc<LazyHash<CompiledSource>>,
        compiler: &Compiler,
    ) -> SourceResult<Self> {
        let params = compiled
            .params()
            .map(|param| match param {
                CompiledParam::Pos(output, name) => {
                    Ok(Param::Pos { name: name.clone(), target: *output })
                }
                CompiledParam::Named { span, target, name, default, .. } => {
                    let Some(default) = default else {
                        return Ok(Param::Named {
                            name: name.clone(),
                            default: None,
                            target: *target,
                        });
                    };

                    let Some(default) = compiler.resolve(*default)
                    else {
                        bail!(*span, "default value not resolved"; hint: "this is a compiler bug");
                    };

                    Ok(Param::Named {
                        name: name.clone(),
                        default: Some(default),
                        target: *target,
                    })
                }
                CompiledParam::Sink(span, dest, _) => {
                    Ok(Param::Sink { span: *span, target: *dest })
                }
            })
            .collect::<SourceResult<_>>()?;

        Ok(Self::new(compiled, params, Vec::new()))
    }
}

impl ClosureInner for Closure {
    fn name(&self) -> Option<&str> {
        self.compiled.name().map(|v| &**v)
    }

    fn dyn_hash(&self, mut state: &mut dyn std::hash::Hasher) {
        self.hash(&mut state);
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

cast! {
    Closure,
    self => Value::Func(self.into()),
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Param {
    /// A positional parameter.
    Pos {
        /// The name of the parameter.
        name: EcoString,
        /// The target in which to store the value.
        target: usize,
    },
    /// A named parameter.
    Named {
        /// The name of the parameter.
        name: EcoString,
        /// The default value of the parameter.
        default: Option<Value>,
        /// The target in which to store the value.
        target: usize,
    },
    /// A sink parameter.
    Sink {
        /// The span of the sink.
        span: Span,
        /// The target in which to store the value.
        ///
        /// No target means that the value is not stored.
        target: Option<usize>,
    },
}

/// A closure that has been compiled but is not yet instantiated.
#[derive(Clone, Hash, PartialEq)]
pub(crate) enum CompiledClosure {
    /// A closure that has been compiled but is not yet instantiated.
    Closure(Arc<LazyHash<CompiledSource>>),
    /// A closure that has been instantiated statically.
    ///
    /// This is used for closures that do not capture any variables.
    /// The closure is already compiled and can be used directly.
    Instanciated(Closure),
}

impl CompiledClosure {
    pub fn new(
        resource: Arc<LazyHash<CompiledSource>>,
        compiler: &Compiler,
    ) -> SourceResult<Self> {
        // Check whether we have any defaults that are resolved at runtime.
        let has_defaults = resource
            .params()
            .filter_map(|param| param.default())
            .any(|default| default.is_dyn());

        // Check if we have any captures.
        let has_captures = resource.captures().next().is_some();

        // If we don't need capturing or initialization, then we can pre-init the closure.
        Ok(if has_defaults || has_captures {
            Self::Closure(resource)
        } else {
            Self::Instanciated(Closure::no_instance(resource, compiler)?)
        })
    }
}
