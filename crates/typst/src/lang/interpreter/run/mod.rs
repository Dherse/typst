mod assign;
mod conditional;
mod flow;
mod markup;
mod math;
mod operators;
mod styling;
mod values;

use typst_syntax::Span;

use crate::diag::SourceResult;
use crate::engine::Engine;
use crate::foundations::{array, Array, IntoValue, Str, Type, Value};
use crate::lang::opcodes::Opcode;

use super::Vm;

pub enum Iterable<'a> {
    /// An iterator over an array
    Array(std::slice::Iter<'a, Value>),
    /// An iterator over the graphemes of a string
    Str(unicode_segmentation::Graphemes<'a>),
    /// An iterator over the bytes of a byte array
    Bytes(std::slice::Iter<'a, u8>),
    /// An iterator over the key-value pairs of a dictionary
    Dict(indexmap::map::Iter<'a, Str, Value>),
}

impl<'a> Iterable<'a> {
    pub fn next(&mut self) -> Option<IteratorValue<'a>> {
        match self {
            Iterable::Array(iter) => iter.next().map(IteratorValue::Array),
            Iterable::Str(iter) => iter.next().map(IteratorValue::Str),
            Iterable::Bytes(iter) => iter.next().copied().map(IteratorValue::Bytes),
            Iterable::Dict(iter) => {
                iter.next().map(|(key, value)| IteratorValue::Dict(key, value))
            }
        }
    }
}

impl<'a> From<std::slice::Iter<'a, Value>> for Iterable<'a> {
    fn from(iter: std::slice::Iter<'a, Value>) -> Self {
        Iterable::Array(iter)
    }
}

impl<'a> From<unicode_segmentation::Graphemes<'a>> for Iterable<'a> {
    fn from(iter: unicode_segmentation::Graphemes<'a>) -> Self {
        Iterable::Str(iter)
    }
}

impl<'a> From<std::slice::Iter<'a, u8>> for Iterable<'a> {
    fn from(iter: std::slice::Iter<'a, u8>) -> Self {
        Iterable::Bytes(iter)
    }
}

impl<'a> From<indexmap::map::Iter<'a, Str, Value>> for Iterable<'a> {
    fn from(iter: indexmap::map::Iter<'a, Str, Value>) -> Self {
        Iterable::Dict(iter)
    }
}

pub enum IteratorValue<'a> {
    Array(&'a Value),
    Str(&'a str),
    Bytes(u8),
    Dict(&'a Str, &'a Value),
}

impl IteratorValue<'_> {
    pub fn type_name(&self) -> &str {
        match self {
            IteratorValue::Array(value) => value.ty().long_name(),
            IteratorValue::Str(_) => Type::of::<Str>().long_name(),
            IteratorValue::Bytes(_) => "bytes",
            IteratorValue::Dict(_, _) => Type::of::<Array>().long_name(),
        }
    }
}

impl IntoValue for IteratorValue<'_> {
    fn into_value(self) -> Value {
        match self {
            IteratorValue::Array(value) => value.clone(),
            IteratorValue::Str(value) => Value::Str(Str::from(value)),
            IteratorValue::Bytes(value) => Value::Int(value as i64),
            IteratorValue::Dict(key, value) => {
                Value::Array(array![key.clone(), value.clone()])
            }
        }
    }
}

/// Runs an individual opcode, while giving it full access to the execution state.
pub trait Run {
    /// Runs an opcode.
    fn run(
        &self,
        instructions: &[Opcode],
        spans: &[Span],
        span: Span,
        vm: &mut Vm,
        engine: &mut Engine,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<()>;
}

/// Runs an individual opcode, while giving it limited access to the execution state.
pub trait SimpleRun {
    /// Runs an opcode.
    fn run(&self, span: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()>;
}

impl<T: SimpleRun> Run for T {
    fn run(
        &self,
        _: &[Opcode],
        _: &[Span],
        span: Span,
        vm: &mut Vm,
        engine: &mut Engine,
        _: Option<&mut Iterable>,
    ) -> SourceResult<()> {
        <T as SimpleRun>::run(self, span, vm, engine)
    }
}
