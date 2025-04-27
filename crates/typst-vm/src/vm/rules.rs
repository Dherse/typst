use typst_library::diag::{bail, At, SourceResult};
use typst_library::foundations::{Recipe, ShowableSelector, Transformation, Value};
use typst_syntax::Span;

use super::call::{make_args, ArgSegment};
use super::flow::Iterable;
use super::{Instruction, Readable, Vm};

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct Set {
    target: Readable,
    args: Vec<ArgSegment>,
    span: Span,
}

impl Set {
    pub fn new(target: Readable, args: Vec<ArgSegment>, span: Span) -> Self {
        Self { target, args, span }
    }
}

impl Instruction for Set {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Load the arguments.
        let args = make_args(vm, &self.args)?;

        // Load the target function.
        let target = match vm.get(self.target, self.span)? {
            Value::Func(func) => {
                if let Some(elem) = func.element() {
                    elem
                } else {
                    bail!(self.span, "only element functions can be used in set rules")
                }
            }
            Value::Type(ty) => {
                if let Some(elem) = ty.constructor().at(self.span)?.element() {
                    elem
                } else {
                    bail!(self.span, "only element functions can be used in set rules")
                }
            }
            other => bail!(self.span, "expected function, found {}", other.ty()),
        };

        // Build the rule and apply it.
        let set_rule = target.set(&mut vm.engine, args)?.spanned(self.span);
        vm.styled(set_rule);

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.target.take_slot(slot);
        self.args.iter_mut().for_each(|arg| arg.take_slot(slot));
    }
}

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct ShowSet {
    selector: Option<Readable>,
    selector_span: Span,
    target: Readable,
    args: Vec<ArgSegment>,
    span: Span,
}

impl ShowSet {
    pub fn new(
        selector: Option<Readable>,
        selector_span: Span,
        target: Readable,
        args: Vec<ArgSegment>,
        span: Span,
    ) -> Self {
        Self { selector, selector_span, target, args, span }
    }
}

impl Instruction for ShowSet {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Load the arguments.
        let args = make_args(vm, &self.args)?;

        // Load the target function.
        let target = match vm.get(self.target, self.span)? {
            Value::Func(func) => {
                if let Some(elem) = func.element() {
                    elem
                } else {
                    bail!(self.span, "only element functions can be used in set rules")
                }
            }
            Value::Type(ty) => {
                if let Some(elem) = ty.constructor().at(self.span)?.element() {
                    elem
                } else {
                    bail!(self.span, "only element functions can be used in set rules")
                }
            }
            other => {
                bail!(self.span, "expected function, found {}", other.ty())
            }
        };

        // Create the show rule.
        let set_rule = target.set(&mut vm.engine, args)?.spanned(self.span);

        // Load the selector.
        let selector = self
            .selector
            .map(|selector| vm.get(selector, self.selector_span))
            .transpose()?
            .map(|v| v.cast::<ShowableSelector>().at(self.selector_span))
            .transpose()?;

        let show_rule = Recipe::new(
            selector.map(|selector| selector.0),
            Transformation::Style(set_rule),
            self.span,
        );

        // Write the value to the output.
        vm.recipe(show_rule);

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.args.iter_mut().for_each(|arg| arg.take_slot(slot));
        self.target.take_slot(slot);
        if let Some(selector) = &mut self.selector {
            selector.take_slot(slot);
        }
    }
}

#[derive(Clone, Copy, PartialEq, Hash, Debug)]
pub struct Show {
    selector: Option<Readable>,
    selector_span: Span,
    transform: Readable,
    span: Span,
}

impl Show {
    pub fn new(
        selector: Option<Readable>,
        selector_span: Span,
        transform: Readable,
        span: Span,
    ) -> Self {
        Self { selector, selector_span, transform, span }
    }
}

impl Instruction for Show {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Load the arguments.
        let transform = vm
            .get(self.transform, self.span)?
            .cast::<Transformation>()
            .at(self.span)?;

        // Load the selector.
        let selector = self
            .selector
            .map(|selector| vm.get(selector, self.selector_span))
            .transpose()?
            .map(|v| v.cast::<ShowableSelector>().at(self.selector_span))
            .transpose()?;

        // Create the show rule.
        let show_rule =
            Recipe::new(selector.map(|selector| selector.0), transform, self.span);

        // Write the value to the output.
        vm.recipe(show_rule);

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.transform.take_slot(slot);

        if let Some(selector) = &mut self.selector {
            selector.take_slot(slot);
        }
    }
}
