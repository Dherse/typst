use typst_library::diag::{bail, At, SourceResult};
use typst_library::foundations::{Array, Dict, Str, Value};
use typst_syntax::Span;

use super::flow::Iterable;
use super::{Instruction, Readable, Vm};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct AllocateArray {
    size_hint: usize,
}

impl AllocateArray {
    pub fn new(size_hint: usize) -> Self {
        Self { size_hint }
    }
}

impl Instruction for AllocateArray {
    type Output = Array;

    fn eval(&self, _: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        Ok(Array::with_capacity(self.size_hint))
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct ArrayPush {
    value: Readable,
    span: Span,
}

impl ArrayPush {
    pub fn new(value: Readable, span: Span) -> Self {
        Self { value, span }
    }
}

impl Instruction for ArrayPush {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let value = vm.get(self.value, self.span)?;

        let top = vm.top_mut().at(self.span)?;
        let Value::Array(array) = top else {
            bail!(
                self.span,
                "excpected an array, found: {}", top.ty().long_name();
                hint: "this is a compiler bug",
            );
        };

        array.push(value);

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.value.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct ArraySpread {
    value: Readable,
    span: Span,
}

impl ArraySpread {
    pub fn new(value: Readable, span: Span) -> Self {
        Self { value, span }
    }
}

impl Instruction for ArraySpread {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let value = vm.get(self.value, self.span)?;

        let top = vm.top_mut().at(self.span)?;
        let Value::Array(array) = top else {
            bail!(
                self.span,
                "excpected an array, found: {}", top.ty();
                hint: "this is a compiler bug",
            );
        };

        match value {
            Value::None => {}
            Value::Array(other) => array.extend(other.into_iter()),
            v => bail!(self.span, "cannot spread {} into array", v.ty()),
        }

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.value.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct AllocateDict {
    size_hint: usize,
}

impl AllocateDict {
    pub fn new(size_hint: usize) -> Self {
        Self { size_hint }
    }
}

impl Instruction for AllocateDict {
    type Output = Dict;

    fn eval(&self, _: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        Ok(Dict::with_capacity(self.size_hint))
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DictInsert {
    key: Str,
    value: Readable,
    span: Span,
}

impl DictInsert {
    pub fn new(key: Str, value: Readable, span: Span) -> Self {
        Self { key, value, span }
    }
}

impl Instruction for DictInsert {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let value = vm.get(self.value, self.span)?;

        let top = vm.top_mut().at(self.span)?;
        let Value::Dict(dict) = top else {
            bail!(
                self.span,
                "excpected a dictionary, found: {}", top.ty().long_name();
                hint: "this is a compiler bug",
            );
        };

        dict.insert(self.key.clone(), value);

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.value.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct DictInsertKeyed {
    key: Readable,
    key_span: Span,
    value: Readable,
    span: Span,
}

impl DictInsertKeyed {
    pub fn new(key: Readable, key_span: Span, value: Readable, span: Span) -> Self {
        Self { key, key_span, value, span }
    }
}

impl Instruction for DictInsertKeyed {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let key = vm.get(self.key, self.span)?.cast::<Str>().at(self.key_span)?;
        let value = vm.get(self.value, self.span)?;

        let top = vm.top_mut().at(self.span)?;
        let Value::Dict(dict) = top else {
            bail!(
                self.span,
                "excpected a dictionary, found: {}", top.ty().long_name();
                hint: "this is a compiler bug",
            );
        };

        dict.insert(key, value);

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.value.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct DictSpread {
    value: Readable,
    span: Span,
}

impl DictSpread {
    pub fn new(value: Readable, span: Span) -> Self {
        Self { value, span }
    }
}

impl Instruction for DictSpread {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let value = vm.get(self.value, self.span)?;

        let top = vm.top_mut().at(self.span)?;
        let Value::Dict(dict) = top else {
            bail!(
                self.span,
                "excpected a dictionary, found: {}", top.ty();
                hint: "this is a compiler bug",
            );
        };

        match value {
            Value::None => {}
            Value::Dict(other) => dict.extend(other.into_iter()),
            v => bail!(self.span, "cannot spread {} into dictionary", v.ty()),
        }

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.value.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Push {
    from: Readable,
    span: Span,
}

impl Push {
    pub fn new(from: Readable, span: Span) -> Self {
        Self { from, span }
    }
}

impl Instruction for Push {
    type Output = Value;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        vm.get(self.from, self.span)
    }

    fn take_slot(&mut self, slot: usize) {
        self.from.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Duplicate {
    span: Span,
}

impl Duplicate {
    pub fn new(span: Span) -> Self {
        Self { span }
    }
}

impl Instruction for Duplicate {
    type Output = Value;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        vm.top().cloned().at(self.span)
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Join {
    value: Readable,
    span: Span,
}

impl Join {
    pub fn new(value: Readable, span: Span) -> Self {
        Self { value, span }
    }
}

impl Instruction for Join {
    type Output = ();

    const FLOWABLE: bool = true;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let value = vm.get(self.value, self.span)?;

        vm.joiner.join(value).at(self.span)?;

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.value.take_slot(slot);
    }
}
