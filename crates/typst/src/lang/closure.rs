use std::borrow::Cow;
use std::sync::Arc;

use typst_macros::cast;
use typst_syntax::Span;
use typst_utils::{LazyHash, PicoStr};

use crate::foundations::Value;

use super::compiled::{CompiledCode, CompiledParam};
use super::compiler::Compiler;
use super::operands::Register;

/// A closure that has been instantiated.
#[derive(Clone, Hash, PartialEq)]
pub struct Closure {
    pub inner: Arc<LazyHash<Repr>>,
}

cast! {
    Closure,
    self => Value::Func(self.into()),
}

#[derive(Hash)]
pub struct Repr {
    /// The compiled code of the closure.
    pub compiled: Arc<LazyHash<CompiledCode>>,
    /// The parameters of the closure.
    pub params: Vec<Param>,
    /// The captured values and where to store them.
    pub captures: Vec<(Register, Value)>,
}

impl Closure {
    /// Creates a new closure.
    pub fn new(
        compiled: Arc<LazyHash<CompiledCode>>,
        params: Vec<Param>,
        captures: Vec<(Register, Value)>,
    ) -> Closure {
        Self {
            inner: Arc::new(LazyHash::new(Repr { compiled, params, captures })),
        }
    }

    /// Get the name of the closure.
    pub fn name(&self) -> Option<&str> {
        self.inner.compiled.name.as_deref()
    }

    pub fn no_instance(compiled: CompiledCode, compiler: &Compiler) -> Self {
        let params = compiled
            .params
            .iter()
            .flat_map(|params| params.iter())
            .map(|param| match param {
                CompiledParam::Pos(output, name) => {
                    Param::Pos { name: *name, target: *output }
                }
                CompiledParam::Named { target, name, default, .. } => {
                    let Some(default) = default else {
                        return Param::Named {
                            name: *name,
                            default: None,
                            target: *target,
                        };
                    };

                    let Some(default) = compiler.resolve(*default).map(Cow::into_owned)
                    else {
                        panic!("default value not resolved, this is a compiler bug.");
                    };

                    Param::Named {
                        name: *name,
                        default: Some(default),
                        target: *target,
                    }
                }
                CompiledParam::Sink(span, dest, _) => {
                    Param::Sink { span: *span, target: *dest }
                }
            })
            .collect();

        Self::new(Arc::new(LazyHash::new(compiled)), params, Vec::new())
    }
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Param {
    /// A positional parameter.
    Pos {
        /// The name of the parameter.
        name: PicoStr,
        /// The target in which to store the value.
        target: Register,
    },
    /// A named parameter.
    Named {
        /// The name of the parameter.
        name: PicoStr,
        /// The default value of the parameter.
        default: Option<Value>,
        /// The target in which to store the value.
        target: Register,
    },
    /// A sink parameter.
    Sink {
        /// The span of the sink.
        span: Span,
        /// The target in which to store the value.
        ///
        /// No target means that the value is not stored.
        target: Option<Register>,
    },
}
