use std::fmt::Debug;
use std::hash::Hash;

use typst_library::diag::{At, SourceResult};
use typst_library::foundations::{ops, Value};
use typst_syntax::Span;

use super::flow::Iterable;
use super::{Instruction, Instructions, MutAccess, Readable, Vm};

macro_rules! binary {
    ($name:ident => $ops:expr) => {
        #[derive(Debug, Clone, Copy, PartialEq, Hash)]
        pub struct $name {
            pub lhs: Readable,
            pub rhs: Readable,
            pub span: Span,
        }

        impl $name {
            pub const fn new(lhs: Readable, rhs: Readable, span: Span) -> Self {
                Self { lhs, rhs, span }
            }
        }

        impl Instruction for $name {
            type Output = Value;

            fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Value> {
                let rhs = vm.get(self.rhs, self.span)?.into_owned();
                let lhs = vm.get(self.lhs, self.span)?.into_owned();
                $ops(lhs, rhs).at(self.span)
            }

            fn take_slot(&mut self, slot: usize) {
                self.lhs.take_slot(slot);
                self.rhs.take_slot(slot);
            }
        }

        impl BinaryOp for $name {
            fn create(lhs: Readable, rhs: Readable, span: Span) -> Self {
                Self::new(lhs, rhs, span)
            }
        }
    };
}

pub trait BinaryOp: Into<Instructions> {
    fn create(lhs: Readable, rhs: Readable, span: Span) -> Self;
}

binary!(Add => ops::add);
binary!(Sub => ops::sub);
binary!(Mul => ops::mul);
binary!(Div => ops::div);
binary!(And => ops::and);
binary!(Or => ops::or);
binary!(Eq => ops::eq);
binary!(Neq => ops::neq);
binary!(Lt => ops::lt);
binary!(Lte => ops::leq);
binary!(Gt => ops::gt);
binary!(Gte => ops::geq);
binary!(In => ops::in_);
binary!(NotIn => ops::not_in);

macro_rules! binary_assign {
    ($name:ident => $ops:expr) => {
        #[derive(Debug, Clone, PartialEq, Hash)]
        pub struct $name {
            pub lhs: MutAccess,
            pub rhs: Readable,
            pub span: Span,
        }

        impl AssignOp for $name {
            fn create(lhs: MutAccess, rhs: Readable, span: Span) -> Self {
                Self { lhs, rhs, span }
            }
        }

        impl Instruction for $name {
            type Output = ();

            fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<()> {
                let rhs = vm.get(self.rhs, self.span)?.into_owned();

                vm.access(&self.lhs, |_vm, location| {
                    let lhs = std::mem::take(&mut *location);
                    *location = $ops(lhs, rhs).at(self.span)?;
                    Ok(())
                })
            }

            fn take_slot(&mut self, slot: usize) {
                self.rhs.take_slot(slot);
            }
        }
    };
}

pub trait AssignOp: Into<Instructions> {
    fn create(lhs: MutAccess, rhs: Readable, span: Span) -> Self;
}

binary_assign!(AddAssign => ops::add);
binary_assign!(SubAssign => ops::sub);
binary_assign!(MulAssign => ops::mul);
binary_assign!(DivAssign => ops::div);

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Assign {
    pub lhs: MutAccess,
    pub rhs: Readable,
    pub span: Span,
}

impl AssignOp for Assign {
    fn create(lhs: MutAccess, rhs: Readable, span: Span) -> Self {
        Self { lhs, rhs, span }
    }
}

impl Instruction for Assign {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<()> {
        let rhs = vm.get(self.rhs, self.span)?.into_owned();

        vm.set(&self.lhs, rhs)
    }

    fn take_slot(&mut self, slot: usize) {
        self.rhs.take_slot(slot);
    }
}
