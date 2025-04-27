use std::num::NonZero;

use typst_library::diag::{bail, At, SourceResult};
use typst_library::foundations::{Content, ContextElem, Label, NativeElement, Value};
use typst_library::model::{
    EmphElem, EnumItem, HeadingElem, ListItem, RefElem, StrongElem, TermItem,
};
use typst_syntax::Span;

use super::flow::Iterable;
use super::{Instruction, Readable, Vm};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Strong {
    body: Readable,
    span: Span,
}

impl Strong {
    pub fn new(body: Readable, span: Span) -> Self {
        Self { body, span }
    }
}

impl Instruction for Strong {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let Value::Content(body) = vm.get(self.body, self.span)? else {
            bail!(self.span, "expected content on stack"; hint: "this is a compiler bug");
        };

        Ok(StrongElem::new(body).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.body.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Emph {
    body: Readable,
    span: Span,
}

impl Emph {
    pub fn new(body: Readable, span: Span) -> Self {
        Self { body, span }
    }
}

impl Instruction for Emph {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let Value::Content(body) = vm.get(self.body, self.span)? else {
            bail!(self.span, "expected content on stack"; hint: "this is a compiler bug");
        };

        Ok(EmphElem::new(body).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.body.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct DynRef {
    target: Label,
    supplement: Readable,
    span: Span,
}

impl DynRef {
    pub fn new(target: Label, supplement: Readable, span: Span) -> Self {
        Self { target, supplement, span }
    }
}

impl Instruction for DynRef {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let Value::Content(supplement) = vm.get(self.supplement, self.span)? else {
            bail!(self.span, "expected content on stack"; hint: "this is a compiler bug");
        };

        Ok(RefElem::new(self.target).with_element(Some(supplement)).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.supplement.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Heading {
    depth: NonZero<usize>,
    body: Readable,
    span: Span,
}

impl Heading {
    pub fn new(depth: NonZero<usize>, body: Readable, span: Span) -> Self {
        Self { depth, body, span }
    }
}

impl Instruction for Heading {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let Value::Content(body) = vm.get(self.body, self.span)? else {
            bail!(self.span, "expected content on stack"; hint: "this is a compiler bug");
        };

        Ok(HeadingElem::new(body).with_depth(self.depth).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.body.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct OpsListItem {
    body: Readable,
    span: Span,
}

impl OpsListItem {
    pub fn new(body: Readable, span: Span) -> Self {
        Self { body, span }
    }
}

impl Instruction for OpsListItem {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let Value::Content(body) = vm.get(self.body, self.span)? else {
            bail!(self.span, "expected content on stack"; hint: "this is a compiler bug");
        };

        Ok(ListItem::new(body).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.body.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct OpsEnumItem {
    body: Readable,
    num: Option<u64>,
    span: Span,
}

impl OpsEnumItem {
    pub fn new(body: Readable, num: Option<u64>, span: Span) -> Self {
        Self { body, num, span }
    }
}

impl Instruction for OpsEnumItem {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let Value::Content(body) = vm.get(self.body, self.span)? else {
            bail!(self.span, "expected content on stack"; hint: "this is a compiler bug");
        };

        Ok(EnumItem::new(body).with_number(self.num).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.body.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct OpsTermItem {
    term: Readable,
    description: Readable,
    span: Span,
}

impl OpsTermItem {
    pub fn new(term: Readable, description: Readable, span: Span) -> Self {
        Self { term, description, span }
    }
}

impl Instruction for OpsTermItem {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Note: the order we 'get' these values from has to be the opposite order
        // has when we compiled them.
        let Value::Content(description) = vm.get(self.description, self.span)? else {
            bail!(self.span, "expected content on stack"; hint: "this is a compiler bug");
        };

        let Value::Content(term) = vm.get(self.term, self.span)? else {
            bail!(self.span, "expected content on stack"; hint: "this is a compiler bug");
        };

        Ok(TermItem::new(term, description).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.term.take_slot(slot);
        self.description.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Contextual {
    closure: Readable,
    span: Span,
}

impl Contextual {
    pub fn new(closure: Readable, span: Span) -> Self {
        Self { closure, span }
    }
}

impl Instruction for Contextual {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let closure = vm.get(self.closure, self.span)?.cast().at(self.span)?;

        Ok(ContextElem::new(closure).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.closure.take_slot(slot);
    }
}
