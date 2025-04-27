use typst_library::diag::SourceResult;
use typst_library::foundations::{Args, Array, Content, Dict, Func, IntoValue, Value};

pub use super::access::FieldAccess;
pub use super::binary::{
    Add, AddAssign, And, Assign, AssignOp, BinaryOp, Div, DivAssign, Eq, Gt, Gte, In, Lt,
    Lte, Mul, MulAssign, Neq, NotIn, Or, Sub, SubAssign,
};
pub use super::call::{FuncCall, MethodCall, MutMethodCall, Reverse};
pub use super::closure::Instantiate;
pub use super::destruct::{Destructure, Write};
pub use super::flow::{
    BreakOp, ContinueOp, Iter, Jump, JumpConditional, Next, ReturnOp, ReturnVal, Scoped,
};
pub use super::markup::{
    Contextual, DynRef, Emph, Heading, OpsEnumItem, OpsListItem, OpsTermItem, Strong,
};
pub use super::math::{Attach, Delimited, Equation, Frac, Root};
pub use super::ops::{
    AllocateArray, AllocateDict, ArrayPush, ArraySpread, DictInsert, DictInsertKeyed,
    DictSpread, Duplicate, Join, Push,
};
pub use super::rules::{Set, Show, ShowSet};
pub use super::unary::{Neg, Not, Pos};
use super::{Iterable, Vm};

macro_rules! instructions {
    ($($name:ident),* $(,)*) => {
        #[derive(Debug, Clone, PartialEq, Hash)]
        pub enum Instructions {
            Noop,
            Flow,
            $($name($name)),*
        }

        impl Instructions {
            pub fn eval(&self, vm: &mut Vm, iterable: Option<&mut Iterable>) -> SourceResult<()> {
                match self {
                    Self::Noop => {},
                    Self::Flow => {},
                    $(
                        Self::$name(op) => op.eval(vm, iterable)?.store(vm)
                    ),*
                }

                Ok(())
            }

            pub fn print_size() {
                $(
                    println!(" - {}: {} bytes", stringify!($name), std::mem::size_of::<$name>());
                )*
            }

            pub const fn is_flow(&self) -> bool {
                match self {
                    Self::Noop => false,
                    Self::Flow => true,
                    $(
                        Self::$name(_) => $name::FLOWABLE
                    ),*
                }
            }

            pub fn take_slot(&mut self, slot: usize) {
                match self {
                    Self::Noop => {},
                    Self::Flow => {},
                    $(
                        Self::$name(op) => op.take_slot(slot)
                    ),*
                }
            }
        }

        $(
            impl From<$name> for Instructions {
                fn from(value: $name) -> Self {
                    Self::$name(value)
                }
            }
        )*
    };
}

pub trait Instruction {
    type Output: InstructionOutput;

    const FLOWABLE: bool = false;

    fn eval(
        &self,
        vm: &mut Vm,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output>;

    fn take_slot(&mut self, slot: usize);
}

pub trait InstructionOutput {
    fn store(self, vm: &mut Vm);
}

impl InstructionOutput for () {
    fn store(self, _: &mut Vm) {}
}

impl InstructionOutput for Content {
    fn store(self, vm: &mut Vm) {
        vm.push(self.into_value());
    }
}

impl InstructionOutput for Value {
    fn store(self, vm: &mut Vm) {
        vm.push(self);
    }
}

impl InstructionOutput for Array {
    fn store(self, vm: &mut Vm) {
        vm.push(self.into_value());
    }
}

impl InstructionOutput for Dict {
    fn store(self, vm: &mut Vm) {
        vm.push(self.into_value());
    }
}

impl InstructionOutput for Args {
    fn store(self, vm: &mut Vm) {
        vm.push(self.into_value());
    }
}

impl InstructionOutput for Func {
    fn store(self, vm: &mut Vm) {
        vm.push(self.into_value());
    }
}

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct Noop;

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct FlowOp;

instructions! {
    FieldAccess,
    Add,
    Sub,
    Mul,
    Div,
    Or,
    And,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    In,
    NotIn,
    Neg,
    Pos,
    Not,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    Assign,
    Reverse,
    FuncCall,
    MethodCall,
    MutMethodCall,
    Instantiate,
    Write,
    Destructure,
    Jump,
    JumpConditional,
    Iter,
    Next,
    Scoped,
    BreakOp,
    ContinueOp,
    ReturnOp,
    ReturnVal,
    Strong,
    Emph,
    DynRef,
    Heading,
    OpsListItem,
    OpsEnumItem,
    OpsTermItem,
    Contextual,
    Equation,
    Delimited,
    Attach,
    Frac,
    Root,
    AllocateArray,
    AllocateDict,
    ArrayPush,
    ArraySpread,
    DictInsert,
    DictInsertKeyed,
    DictSpread,
    Push,
    Duplicate,
    Join,
    Set,
    ShowSet,
    Show,
}

impl From<Noop> for Instructions {
    fn from(_: Noop) -> Self {
        Self::Noop
    }
}

impl From<FlowOp> for Instructions {
    fn from(_: FlowOp) -> Self {
        Self::Flow
    }
}
