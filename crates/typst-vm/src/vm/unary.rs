use typst_library::diag::{At, SourceResult};
use typst_library::foundations::{ops, Value};
use typst_syntax::Span;

use super::{Instruction, Iterable, Readable, Vm};

macro_rules! unary {
    ($name:ident => $ops:expr) => {
        #[derive(Debug, Clone, Copy, PartialEq, Hash)]
        pub struct $name {
            pub rhs: Readable,
            pub span: Span,
        }

        impl $name {
            pub const fn new(rhs: Readable) -> Self {
                Self { rhs, span: Span::detached() }
            }

            pub const fn at(mut self, span: Span) -> Self {
                self.span = span;
                self
            }
        }

        impl Instruction for $name {
            type Output = Value;

            fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Value> {
                let value = vm.get(self.rhs, self.span)?.into_owned();
                $ops(value).at(self.span)
            }

            fn take_slot(&mut self, slot: usize) {
                self.rhs.take_slot(slot);
            }
        }
    };
}

unary!(Neg => ops::neg);
unary!(Pos => ops::pos);
unary!(Not => ops::not);
