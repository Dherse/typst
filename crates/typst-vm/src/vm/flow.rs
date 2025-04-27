use typst_library::diag::{bail, At, SourceResult};
use typst_library::foundations::{array, Array, IntoValue, Str, Type, Value};
use typst_syntax::Span;
use unicode_segmentation::UnicodeSegmentation;

use crate::compiler::{Pointer, PointerRange};
use crate::vm::ControlFlow;

use super::destruct::Pattern;
use super::{Instruction, Readable};

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct Jump {
    end: Pointer,
    span: Span,
}

impl Jump {
    pub fn new(end: Pointer, span: Span) -> Self {
        Self { end, span }
    }
}

impl Instruction for Jump {
    type Output = ();

    fn eval(
        &self,
        vm: &mut super::Vm,
        _: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        vm.jump(self.end, false).at(self.span)?;

        Ok(())
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct JumpConditional {
    end: Pointer,
    condition: Readable,
    target: bool,
    convergent: Option<Span>,
    inc: Option<Span>,
    span: Span,
}

impl JumpConditional {
    pub fn new(end: Pointer, condition: Readable, target: bool, span: Span) -> Self {
        Self {
            end,
            condition,
            target,
            convergent: None,
            inc: None,
            span,
        }
    }

    pub fn with_convergence_check(mut self, convergent: Option<Span>) -> Self {
        self.convergent = convergent;
        self
    }

    pub fn with_increment(mut self, inc: Option<Span>) -> Self {
        self.inc = inc;
        self
    }
}

impl Instruction for JumpConditional {
    type Output = ();

    fn eval(
        &self,
        vm: &mut super::Vm,
        _: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        let value = vm.get(self.condition, self.span)?.cast::<bool>().at(self.span)?;

        // Check for convergence (i.e value is always true)
        if value {
            if let Some(span) = self.convergent {
                bail!(span, "condition is always true")
            }
        }

        // Conditionally jump.
        if value == self.target {
            vm.jump(self.end, false).at(self.span)?;
        }

        if let Some(span) = self.inc {
            vm.inc().at(span)?;
        }

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.condition.take_slot(slot);
    }
}

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct Iter {
    iterable: Readable,
    range: PointerRange,
    span: Span,
    content: bool,
    destructuring: Option<Span>,
}

impl Iter {
    pub fn new(
        iterable: Readable,
        range: PointerRange,
        span: Span,
        content: bool,
        destructuring: Option<Span>,
    ) -> Self {
        Self { iterable, range, span, content, destructuring }
    }
}

impl Instruction for Iter {
    type Output = Value;

    fn eval(
        &self,
        vm: &mut super::Vm,
        _: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        let iterable = vm.get(self.iterable, self.span)?;

        macro_rules! iter {
            (for $iterable:expr) => {{
                let mut iter = Iterable::from($iterable.into_iter());
                vm.enter_scope(self.range, Some(&mut iter), None, self.content, true)?
            }};
        }

        let iterable_type = iterable.ty();
        let flow = match iterable {
            Value::Array(array) => {
                // Iterate over values of array.
                iter!(for array.iter())
            }
            Value::Dict(dict) => {
                // Iterate over key-value pairs of dict.
                iter!(for dict.iter())
            }
            Value::Str(str) => {
                if let Some(span) = self.destructuring {
                    bail!(span, "cannot destructure values of string");
                }

                // Iterate over graphemes of string.
                iter!(for str.as_str().graphemes(true))
            }
            Value::Bytes(bytes) => {
                if let Some(span) = self.destructuring {
                    bail!(span, "cannot destructure values of bytes");
                }

                // Iterate over the integers of bytes.
                iter!(for bytes.as_slice().iter())
            }
            _ => {
                bail!(self.span, "cannot loop over {}", iterable_type);
            }
        };

        let output = match flow {
            ControlFlow::Done(value) => value,
            ControlFlow::Break(_) | ControlFlow::Continue(_) => {
                unreachable!("unexpected control flow")
            }
            ControlFlow::Return(value, forced) => {
                vm.state.set_returning(forced);
                value
            }
        };

        // Jump after the for loop.
        vm.jump(self.range.end(), false).at(self.span)?;

        Ok(output)
    }

    fn take_slot(&mut self, slot: usize) {
        self.iterable.take_slot(slot);
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Next {
    span: Span,
    pattern: Pattern,
}

impl Next {
    pub fn new(span: Span, pattern: Pattern) -> Self {
        Self { span, pattern }
    }
}

impl Instruction for Next {
    type Output = ();

    fn eval(
        &self,
        vm: &mut super::Vm,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        let Some(iter) = iterator else {
            bail!(self.span, "not in an iterable scope"; hint: "this is a compiler bug");
        };

        // Get the next value.
        let Some(value) = iter.next() else {
            vm.state.set_done();
            return Ok(());
        };

        self.pattern.write(vm, value.into_value())?;

        Ok(())
    }

    fn take_slot(&mut self, _: usize) {}
}

pub enum Iterable<'a> {
    /// An iterator over an array
    Array(std::slice::Iter<'a, Value>),
    /// An iterator over the graphemes of a string
    Str(unicode_segmentation::Graphemes<'a>),
    /// An iterator over the bytes of a byte array
    Bytes(std::slice::Iter<'a, u8>),
    /// An iterator over the key-value pairs of a dictionary
    Dict(indexmap::map::Iter<'a, Str, Value>),
    Borrowed(&'a mut Self),
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
            Self::Borrowed(borrowed) => borrowed.next(),
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

#[derive(Debug)]
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

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct Scoped {
    range: PointerRange,
    span: Span,
    content: bool,
    looping: bool,
}

impl Scoped {
    pub fn new(range: PointerRange, span: Span, content: bool, looping: bool) -> Self {
        Self { range, span, content, looping }
    }
}

impl Instruction for Scoped {
    type Output = Value;

    const FLOWABLE: bool = true;

    fn eval(
        &self,
        vm: &mut super::Vm,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        let flow =
            vm.enter_scope(self.range, iterator, None, self.content, self.looping)?;

        let output = if self.looping {
            match flow {
                ControlFlow::Done(value) => value,
                ControlFlow::Break(_) | ControlFlow::Continue(_) => {
                    bail!(self.span, "unexpected control flow"; hint: "this is a compiler bug")
                }
                ControlFlow::Return(value, forced) => {
                    vm.state.set_returning(forced);
                    value
                }
            }
        } else {
            let mut forced_return = false;
            let output = match flow {
                ControlFlow::Done(value) => value,
                ControlFlow::Break(value) => {
                    vm.state.set_breaking();
                    value
                }
                ControlFlow::Continue(value) => {
                    vm.state.set_continuing();
                    value
                }
                ControlFlow::Return(value, forced) => {
                    vm.state.set_returning(forced);
                    forced_return = forced;
                    value
                }
            };

            if forced_return {
                vm.output = Some((self.span, Readable::Stack));
            }

            output
        };

        Ok(output)
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct BreakOp;

impl Instruction for BreakOp {
    type Output = ();

    const FLOWABLE: bool = true;

    fn eval(
        &self,
        vm: &mut super::Vm,
        _: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        if !vm.state.is_continuing() && !vm.state.is_returning() {
            vm.state.set_breaking();
        }

        Ok(())
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct ContinueOp;

impl Instruction for ContinueOp {
    type Output = ();

    const FLOWABLE: bool = true;

    fn eval(
        &self,
        vm: &mut super::Vm,
        _: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        if !vm.state.is_breaking() && !vm.state.is_returning() {
            vm.state.set_continuing();
        }

        Ok(())
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct ReturnOp;

impl Instruction for ReturnOp {
    type Output = ();

    const FLOWABLE: bool = true;

    fn eval(
        &self,
        vm: &mut super::Vm,
        _: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        if !vm.state.is_breaking() && !vm.state.is_continuing() {
            vm.state.set_returning(vm.output.is_some());
        }

        Ok(())
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct ReturnVal {
    value: Readable,
    span: Span,
}

impl ReturnVal {
    pub fn new(value: Readable, span: Span) -> Self {
        Self { value, span }
    }
}

impl Instruction for ReturnVal {
    type Output = ();

    const FLOWABLE: bool = true;

    fn eval(
        &self,
        vm: &mut super::Vm,
        _: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        vm.output = Some((self.span, self.value));

        if !vm.state.is_breaking() && !vm.state.is_continuing() {
            vm.state.set_returning(vm.output.is_some());
        }

        Ok(())
    }

    fn take_slot(&mut self, _: usize) {}
}
