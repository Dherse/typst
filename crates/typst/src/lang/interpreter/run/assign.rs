use typst_syntax::Span;

use crate::diag::{At, HintedStrResult, SourceResult};
use crate::engine::Engine;
use crate::foundations::Value;
use crate::lang::interpreter::Vm;
use crate::lang::opcodes::{
    AddAssign, Assign, Destructure, DivAssign, MulAssign, SubAssign,
};
use crate::lang::operands::{AccessId, Readable};
use crate::lang::ops;

use super::SimpleRun;

impl SimpleRun for Assign {
    fn run(&self, span: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        assign(span, vm, engine, self.value, self.out)
    }
}

impl SimpleRun for AddAssign {
    fn run(&self, _: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        let lhs_span = vm.read(self.lhs_span);
        assign_op(lhs_span, vm, engine, self.value, self.out, |old, value| {
            ops::add(&old, &value)
        })
    }
}

impl SimpleRun for SubAssign {
    fn run(&self, _: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        let lhs_span = vm.read(self.lhs_span);
        assign_op(lhs_span, vm, engine, self.value, self.out, |old, value| {
            ops::sub(&old, &value)
        })
    }
}

impl SimpleRun for MulAssign {
    fn run(&self, _: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        let lhs_span = vm.read(self.lhs_span);
        assign_op(lhs_span, vm, engine, self.value, self.out, |old, value| {
            ops::mul(&old, &value)
        })
    }
}

impl SimpleRun for DivAssign {
    fn run(&self, _: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        let lhs_span = vm.read(self.lhs_span);
        assign_op(lhs_span, vm, engine, self.value, self.out, |old, value| {
            ops::div(&old, &value)
        })
    }
}

impl SimpleRun for Destructure {
    fn run(&self, _: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        // Get the value.
        let value = vm.read_or_clone(self.value);

        // Get the pattern.
        let pattern = vm.read(self.out);

        // Destructure the value.
        pattern.write(vm, engine, &value)?;

        Ok(())
    }
}

fn assign(
    span: Span,
    vm: &mut Vm,
    engine: &mut Engine,
    value: Readable,
    out: AccessId,
) -> SourceResult<()> {
    // Get the value.
    let value = vm.read(value).clone();

    // Get the accessor.
    let access = vm.read(out);

    // Get the mutable reference to the target.
    access.get(vm, engine, true, |location| location.write(value).at(span))?;

    Ok(())
}

fn assign_op(
    span: Span,
    vm: &mut Vm,
    engine: &mut Engine,
    value: Readable,
    out: AccessId,
    transformer: impl FnOnce(Value, Value) -> HintedStrResult<Value>,
) -> SourceResult<()> {
    // Get the value.
    let value = vm.read(value).clone();

    // Get the accessor.
    let access = vm.read(out);

    // Get the mutable reference to the target.
    access.get(vm, engine, true, |location| {
        location.write_transformed(|old| transformer(old, value)).at(span)
    })?;

    Ok(())
}
