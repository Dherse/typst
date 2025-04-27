use std::borrow::Cow;
use std::sync::Arc;

use typst_library::diag::SourceResult;
use typst_library::foundations::Func;
use typst_syntax::Span;
use typst_utils::LazyHash;

use crate::closure::{Closure, Param};
use crate::{CompiledParam, CompiledSource};

use super::flow::Iterable;
use super::Instruction;

#[derive(Clone, PartialEq, Debug, Hash)]
pub(crate) struct Instantiate {
    closure: Arc<LazyHash<CompiledSource>>,
    span: Span,
}

impl Instantiate {
    pub fn new(closure: Arc<LazyHash<CompiledSource>>, span: Span) -> Self {
        Self { closure, span }
    }
}

impl Instruction for Instantiate {
    type Output = Func;

    fn eval(
        &self,
        vm: &mut super::Vm,
        _: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        // Load the default values for the parameters.
        let arg_count = self.closure.params.as_ref().map_or(0, |p| p.len());
        let mut params = Vec::with_capacity(arg_count);
        for param in self.closure.params.iter().flat_map(|p| p.iter()) {
            match param {
                CompiledParam::Pos(target, pos) => {
                    params.push(Param::Pos { name: pos.clone(), target: *target });
                }
                CompiledParam::Named { span, target, name, default } => {
                    params.push(Param::Named {
                        name: name.clone(),
                        default: default
                            .map(|d| vm.get(d, *span))
                            .transpose()?
                            .map(Cow::into_owned),
                        target: *target,
                    });
                }
                CompiledParam::Sink(span, target, _) => {
                    params.push(Param::Sink { span: *span, target: *target });
                }
            }
        }

        // Load the captured values.
        let capture_count = self.closure.captures.as_ref().map_or(0, |c| c.len());
        let mut captures = Vec::with_capacity(capture_count);
        for capture in self.closure.captures.iter().flat_map(|c| c.iter()) {
            captures
                .push((capture.slot, vm.get(capture.readable, self.span)?.into_owned()));
        }

        Ok(Func::from(Closure::new(self.closure.clone(), params, captures)))
    }

    fn take_slot(&mut self, _: usize) {}
}
