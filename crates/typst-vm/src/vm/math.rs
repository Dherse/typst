use typst_library::diag::{At, SourceResult};
use typst_library::foundations::{Content, NativeElement};
use typst_library::math::{AttachElem, EquationElem, FracElem, LrElem, RootElem};
use typst_syntax::Span;

use super::flow::Iterable;
use super::{Instruction, Readable, Vm};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Equation {
    body: Readable,
    block: bool,
    span: Span,
}

impl Equation {
    pub fn new(body: Readable, block: bool, span: Span) -> Self {
        Self { body, block, span }
    }
}

impl Instruction for Equation {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Obtain the value.
        let body = vm.get(self.body, self.span)?.display().spanned(self.span);

        // Make the value into an equation.
        Ok(EquationElem::new(body).with_block(self.block).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.body.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Delimited {
    open: Readable,
    open_span: Span,
    body: Readable,
    body_span: Span,
    close: Readable,
    close_span: Span,
}

impl Delimited {
    pub fn new(
        open: Readable,
        open_span: Span,
        body: Readable,
        body_span: Span,
        close: Readable,
        close_span: Span,
    ) -> Self {
        Self {
            open,
            open_span,
            body,
            body_span,
            close,
            close_span,
        }
    }
}

impl Instruction for Delimited {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Obtain the values.
        let close = vm
            .get(self.close, self.close_span)?
            .display()
            .spanned(self.close_span);
        let body = vm
            .get(self.body, self.body_span)?
            .cast::<Content>()
            .at(self.body_span)?;
        let open = vm.get(self.open, self.open_span)?.display().spanned(self.open_span);

        // Make the value into an LrElem.
        Ok(LrElem::new(open + body + close).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.open.take_slot(slot);
        self.body.take_slot(slot);
        self.close.take_slot(slot);
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Attach(Box<AttachRepr>);

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
struct AttachRepr {
    base: Readable,
    base_span: Span,
    top: Option<Readable>,
    top_span: Span,
    primes: Option<Readable>,
    primes_span: Span,
    bottom: Option<Readable>,
    bottom_span: Span,
    span: Span,
}

impl Attach {
    pub fn new(
        base: Readable,
        base_span: Span,
        top: Option<Readable>,
        top_span: Span,
        primes: Option<Readable>,
        primes_span: Span,
        bottom: Option<Readable>,
        bottom_span: Span,
        span: Span,
    ) -> Self {
        Self(Box::new(AttachRepr {
            base,
            base_span,
            top,
            top_span,
            primes,
            primes_span,
            bottom,
            bottom_span,
            span,
        }))
    }
}

impl Instruction for Attach {
    type Output = Content;

    fn eval(
        &self,
        vm: &mut Vm,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        self.0.eval(vm, iterator)
    }

    fn take_slot(&mut self, slot: usize) {
        self.0.take_slot(slot);
    }
}

impl Instruction for AttachRepr {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Obtain the base, top, and bottom.
        let bottom = self
            .bottom
            .map(|bottom| vm.get(bottom, self.bottom_span))
            .transpose()?;
        let primes = self
            .primes
            .map(|primes| vm.get(primes, self.primes_span))
            .transpose()?;
        let top = self.top.map(|top| vm.get(top, self.top_span)).transpose()?;
        let base = vm.get(self.base, self.base_span)?;

        // Make the value into an attach.
        let mut value = AttachElem::new(base.clone().display());

        if let Some(top) = top {
            value.push_t(Some(top.display().spanned(self.top_span)));
        }

        if let Some(primes) = primes {
            value.push_tr(Some(primes.display().spanned(self.primes_span)));
        }

        if let Some(bottom) = bottom {
            value.push_b(Some(bottom.display().spanned(self.bottom_span)));
        }

        // Write the value to the output.
        Ok(value.pack().spanned(self.span))
    }

    fn take_slot(&mut self, slot: usize) {
        self.base.take_slot(slot);

        if let Some(top) = &mut self.top {
            top.take_slot(slot);
        }

        if let Some(primes) = &mut self.primes {
            primes.take_slot(slot);
        }

        if let Some(bottom) = &mut self.bottom {
            bottom.take_slot(slot);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Frac {
    num: Readable,
    num_span: Span,
    denom: Readable,
    denom_span: Span,
}

impl Frac {
    pub fn new(num: Readable, num_span: Span, denom: Readable, denom_span: Span) -> Self {
        Self { num, num_span, denom, denom_span }
    }
}

impl Instruction for Frac {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Obtain the numerator & denominator
        let denom = vm
            .get(self.denom, self.denom_span)?
            .display()
            .spanned(self.denom_span);
        let num = vm.get(self.num, self.num_span)?.display().spanned(self.num_span);

        // Write the value to the output.
        Ok(FracElem::new(num, denom).pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.denom.take_slot(slot);
        self.num.take_slot(slot);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Root {
    radicand: Readable,
    radicand_span: Span,
    degree: Option<Readable>,
    degree_span: Span,
}

impl Root {
    pub fn new(
        radicand: Readable,
        radicand_span: Span,
        degree: Option<Readable>,
        degree_span: Span,
    ) -> Self {
        Self { radicand, radicand_span, degree, degree_span }
    }
}

impl Instruction for Root {
    type Output = Content;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        // Obtain the degree and radicand.
        let degree = self.degree.map(|d| vm.get(d, self.degree_span)).transpose()?;
        let radicand = vm.get(self.radicand, self.radicand_span)?;

        // Make the value into a root.
        let mut value = RootElem::new(radicand.display());

        if let Some(degree) = degree {
            value.push_index(Some(degree.display()));
        }

        // Write the value to the output.
        Ok(value.pack())
    }

    fn take_slot(&mut self, slot: usize) {
        self.radicand.take_slot(slot);
        if let Some(degree) = &mut self.degree {
            degree.take_slot(slot);
        }
    }
}
