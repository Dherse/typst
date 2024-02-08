use std::fmt;

use ecow::EcoString;
use typst_syntax::Span;

use crate::compiler::CompiledClosure;
use crate::diag::{bail, StrResult};
use crate::foundations::{IntoValue, Label, Value};

use super::{Access, Pattern, VMState, VmStorage};

pub trait VmRead {
    type Output<'a, 'b>
    where
        'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b>;
}

impl<T: VmRead> VmRead for Option<T> {
    type Output<'a, 'b> = Option<T::Output<'a, 'b>> where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        if let Some(this) = self {
            Some(this.read(vm))
        } else {
            None
        }
    }
}

pub trait VmWrite {
    fn write<'a>(&self, vm: &'a mut VMState) -> &'a mut Value;

    fn write_one(self, vm: &mut VMState, value: impl IntoValue) -> StrResult<()>
    where
        Self: Sized,
    {
        *self.write(vm) = value.into_value();
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Readable {
    Const(Constant),
    Reg(Register),
    Str(StringId),
    Global(Global),
    Math(Math),
    None,
    Auto,
    Bool(bool),
}

impl Readable {
    pub fn is_reg(self) -> bool {
        matches!(self, Self::Reg(_))
    }
}

impl VmRead for Readable {
    type Output<'a, 'b> = &'b Value where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        match self {
            Self::Const(constant) => constant.read(vm),
            Self::Reg(register) => register.read(vm),
            Self::Str(string) => string.read(vm),
            Self::Global(global) => global.read(vm),
            Self::Math(math) => math.read(vm),
            Self::None => &Value::None,
            Self::Auto => &Value::Auto,
            Self::Bool(value) => {
                if *value {
                    &Value::Bool(true)
                } else {
                    &Value::Bool(false)
                }
            }
        }
    }
}

impl Readable {
    /// Creates a new none readable.
    pub const fn none() -> Self {
        Self::None
    }

    /// Creates a new auto readable.
    pub const fn auto() -> Self {
        Self::Auto
    }

    /// Creates a new bool readable.
    pub const fn bool(value: bool) -> Self {
        Self::Bool(value)
    }

    /// Creates a new constant readable.
    pub const fn const_(const_: Constant) -> Self {
        Self::Const(const_)
    }

    /// Creates a new register readable.
    pub const fn reg(reg: Register) -> Self {
        Self::Reg(reg)
    }

    /// Creates a new string readable.
    pub const fn string(string: StringId) -> Self {
        Self::Str(string)
    }

    /// Creates a new global readable.
    pub const fn global(global: Global) -> Self {
        Self::Global(global)
    }

    /// Creates a new math readable.
    pub const fn math(math: Math) -> Self {
        Self::Math(math)
    }

    /// Returns this readable as a constant.
    pub fn as_const(self) -> Constant {
        match self {
            Self::Const(constant) => constant,
            _ => unreachable!(),
        }
    }

    /// Returns this readable as a register.
    pub fn as_reg(self) -> Register {
        match self {
            Self::Reg(register) => register,
            _ => unreachable!(),
        }
    }

    /// Returns this readable as a string.
    pub fn as_string(self) -> StringId {
        match self {
            Self::Str(string) => string,
            _ => unreachable!(),
        }
    }

    /// Returns this readable as a global.
    pub fn as_global(self) -> Global {
        match self {
            Self::Global(global) => global,
            _ => unreachable!(),
        }
    }

    /// Returns this readable as a math.
    pub fn as_math(self) -> Math {
        match self {
            Self::Math(math) => math,
            _ => unreachable!(),
        }
    }

    /// Returns this readable as a bool.
    pub fn as_bool(self) -> bool {
        match self {
            Self::Bool(value) => value,
            _ => unreachable!(),
        }
    }
}

impl fmt::Debug for Readable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const(constant) => constant.fmt(f),
            Self::Reg(register) => register.fmt(f),
            Self::Str(string) => string.fmt(f),
            Self::Global(global) => global.fmt(f),
            Self::Math(math) => math.fmt(f),
            Self::None => write!(f, "none"),
            Self::Auto => write!(f, "auto"),
            Self::Bool(value) => write!(f, "{value}"),
        }
    }
}

impl From<Constant> for Readable {
    fn from(constant: Constant) -> Self {
        Self::const_(constant)
    }
}

impl From<Register> for Readable {
    fn from(register: Register) -> Self {
        Self::reg(register)
    }
}

impl From<StringId> for Readable {
    fn from(string: StringId) -> Self {
        Self::string(string)
    }
}

impl From<Global> for Readable {
    fn from(global: Global) -> Self {
        Self::global(global)
    }
}

impl From<Math> for Readable {
    fn from(math: Math) -> Self {
        Self::math(math)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Writable {
    Reg(Register),
    Joined,
}

impl Writable {
    /// Creates a new register writeable.
    pub const fn reg(reg: Register) -> Self {
        Self::Reg(reg)
    }

    /// Creates a new joined writeable.
    pub const fn joined() -> Self {
        Self::Joined
    }

    /// Returns this writeable as a register.
    pub const fn as_reg(self) -> Register {
        match self {
            Self::Reg(register) => register,
            _ => unreachable!(),
        }
    }
}

impl fmt::Debug for Writable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg(register) => register.fmt(f),
            Self::Joined => write!(f, "J"),
        }
    }
}

impl From<Register> for Writable {
    fn from(register: Register) -> Self {
        Self::reg(register)
    }
}

impl VmWrite for Writable {
    fn write<'a, 'b>(&self, vm: &'b mut VMState<'a, '_>) -> &'b mut Value {
        match self {
            Self::Reg(register) => register.write(vm),
            Self::Joined => unreachable!("cannot get mutable reference to joined value"),
        }
    }

    fn write_one<'a>(
        self,
        vm: &mut VMState<'a, '_>,
        value: impl IntoValue,
    ) -> StrResult<()>
    where
        Self: Sized,
    {
        match self {
            Self::Reg(register) => register.write_one(vm, value),
            Self::Joined => vm.join(value),
        }
    }
}

impl VmRead for Writable {
    type Output<'a, 'b> = &'b Value where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        match self {
            Self::Reg(register) => register.read(vm),
            Self::Joined => unreachable!("cannot read joined value"),
        }
    }
}

impl TryInto<Readable> for Writable {
    type Error = EcoString;

    fn try_into(self) -> Result<Readable, Self::Error> {
        match self {
            Self::Reg(register) => Ok(Readable::from(register)),
            Self::Joined => bail!("cannot convert joined value to readable"),
        }
    }
}

macro_rules! id {
    (
        $(#[$sattr:meta])*
        $name:ident($type:ty) => $l:literal
    ) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        #[repr(transparent)]
        pub struct $name(pub $type);

        impl $name {
            pub fn new(index: $type) -> Self {
                Self(index)
            }

            pub const fn as_raw(self) -> $type {
                self.0
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, concat!($l, "{:?}"), self.0)
            }
        }
    };
    (
        $(#[$attr:meta])*
        $name:ident => $l:literal
    ) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        #[repr(transparent)]
        pub struct $name(pub u16);

        impl $name {
            pub const fn new(index: u16) -> Self {
                Self(index)
            }

            pub const fn as_raw(self) -> u16 {
                self.0
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, concat!($l, "{}"), self.0)
            }
        }
    };
    ($(
        $(#[$attr:meta])*
        $name:ident$(($type:ty))? => $l:literal
    ),* $(,)*) => {
        $( id!(
            $(#[$attr])*
            $name$(($type))? => $l
        ); )*
    };
}

id! {
    Constant => "C",
    Register => "R",
    StringId => "S",
    ClosureId => "F",
    AccessId => "A",
    Global => "G",
    Math => "M",
    Pointer => "P",
    LabelId => "L",
    CallId => "K",
    PatternId => "D",
    SpanId => "N",
}

impl VmRead for Constant {
    type Output<'a, 'b> = &'a Value where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        &vm.code.constants[self.0 as usize]
    }
}

impl VmRead for Register {
    type Output<'a, 'b> = &'b Value where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        &vm.registers.read(self.0 as usize)
    }
}

impl VmWrite for Register {
    fn write<'a, 'b>(&self, vm: &'b mut VMState<'a, '_>) -> &'b mut Value {
        vm.registers.write(self.0 as usize)
    }
}

impl VmRead for StringId {
    type Output<'a, 'b> = &'a Value where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        &vm.code.strings[self.0 as usize]
    }
}

impl VmRead for ClosureId {
    type Output<'a, 'b> = &'a CompiledClosure where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        &vm.code.closures[self.0 as usize]
    }
}

impl VmRead for Global {
    type Output<'a, 'b> = &'a Value where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        vm.code.global.global.field_by_id(self.0 as usize).unwrap()
    }
}

impl VmRead for Math {
    type Output<'a, 'b> = &'a Value where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        vm.code.global.math.field_by_id(self.0 as usize).unwrap()
    }
}

impl VmRead for LabelId {
    type Output<'a, 'b> = Label where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        vm.code.labels[self.0 as usize]
    }
}

impl VmRead for AccessId {
    type Output<'a, 'b> = &'a Access where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        &vm.code.accesses[self.0 as usize]
    }
}

impl VmRead for PatternId {
    type Output<'a, 'b> = &'a Pattern where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        &vm.code.patterns[self.0 as usize]
    }
}

impl VmRead for SpanId {
    type Output<'a, 'b> = Span where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        vm.code.isr_spans[self.0 as usize]
    }
}

impl VmRead for Pointer {
    type Output<'a, 'b> = usize where 'a: 'b;

    fn read<'a, 'b>(&self, vm: &'b VMState<'a, '_>) -> Self::Output<'a, 'b> {
        vm.code.jumps[self.0 as usize]
    }
}
