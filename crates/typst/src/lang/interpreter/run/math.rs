use typst_syntax::Span;

use crate::diag::{At, SourceResult};
use crate::engine::Engine;
use crate::foundations::{Content, NativeElement, SequenceElem};
use crate::lang::interpreter::Vm;
use crate::lang::opcodes::{Attach, Delimited, Equation, Frac, Root};
use crate::math::{AttachElem, EquationElem, FracElem, LrElem, RootElem};

use super::SimpleRun;

impl SimpleRun for Delimited {
    fn run(&self, span: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        // Obtain the left delimiter, body, and right delimiter.
        let left: Content = vm.read(self.left).clone().display();
        let body: Content = vm.read(self.body).clone().display();
        let right: Content = vm.read(self.right).clone().display();

        // Make the value into a delimited.
        let value =
            LrElem::new(SequenceElem::new(vec![left, body, right]).pack().spanned(span));

        // Write the value to the output.
        vm.write_one(self.out, value.pack().spanned(span)).at(span)?;

        Ok(())
    }
}

impl SimpleRun for Attach {
    fn run(&self, span: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        // Obtain the base, top, and bottom.
        let base = vm.read(self.base);
        let top = vm.read(self.top);
        let primes = vm.read(self.primes);
        let bottom = vm.read(self.bottom);

        // Make the value into an attach.
        let mut value = AttachElem::new(base.clone().display());

        if let Some(top) = top {
            value.push_t(Some(top.clone().display()));
        } else if let Some(primes) = primes {
            value.push_tr(Some(primes.clone().display()));
        }

        if let Some(bottom) = bottom {
            value.push_b(Some(bottom.clone().display()));
        }

        // Write the value to the output.
        vm.write_one(self.out, value.pack().spanned(span)).at(span)?;

        Ok(())
    }
}

impl SimpleRun for Frac {
    fn run(&self, span: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        // Obtain the numerator and denominator.
        let numerator = vm.read(self.numerator);
        let denominator = vm.read(self.denominator);

        // Make the value into a fraction.
        let value =
            FracElem::new(numerator.clone().display(), denominator.clone().display());

        // Write the value to the output.
        vm.write_one(self.out, value.pack().spanned(span)).at(span)?;

        Ok(())
    }
}

impl SimpleRun for Root {
    fn run(&self, span: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        // Obtain the degree and radicand.
        let degree = vm.read(self.degree);
        let radicand = vm.read(self.radicand);

        // Make the value into a root.
        let mut value = RootElem::new(radicand.clone().display());

        if let Some(degree) = degree {
            value.push_index(Some(degree.clone().display()));
        }

        // Write the value to the output.
        vm.write_one(self.out, value.pack().spanned(span)).at(span)?;

        Ok(())
    }
}

impl SimpleRun for Equation {
    fn run(&self, span: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        // Obtain the value.
        let value = vm.read(self.value);

        // Make the value into an equation.
        let value =
            EquationElem::new(value.clone().cast().at(span)?).with_block(self.block);

        // Write the value to the output.
        vm.write_one(self.out, value.pack().spanned(span)).at(span)?;

        Ok(())
    }
}

/*impl SimpleRun for CallMath {
    fn run(&self, span: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        // Try calling the function.
        let call = Call { closure: self.fallback, out: self.out };

        let err = match call.run(span, vm, engine) {
            Ok(()) => return Ok(()),
            Err(e) => e,
        };

        // Get the arguments.
        let args = match self.args {
            Readable::Reg(reg) => vm.take(reg).into_owned(),
            other => vm.read(other).clone(),
        };

        let mut args = match args {
            Value::None => Args::new::<Value>(span, []),
            Value::Args(args) => args,
            _ => {
                bail!(
                    span,
                    "expected arguments or none, found {}",
                    args.ty().long_name()
                );
            }
        };

        // Get the callee.
        let callee = vm.read(self.callee);

        if let Value::Symbol(sym) = &*callee {
            let c = sym.get();
            if let Some(accent) = Symbol::combining_accent(c) {
                let base = args.expect("base")?;
                let size = args.named("size")?;
                args.finish()?;
                let mut accent = AccentElem::new(base, Accent::new(accent));
                if let Some(size) = size {
                    accent = accent.with_size(size);
                }

                // Write the value to the output.
                vm.write_one(self.out, accent.pack().spanned(span)).at(span)?;

                return Ok(());
            }
        }

        let mut body = Content::empty();
        for (i, arg) in args.all::<Content>()?.into_iter().enumerate() {
            if i > 0 {
                body += TextElem::packed(',');
            }
            body += arg;
        }

        if self.trailing_comma {
            body += TextElem::packed(',');
        }

        let out = callee.clone().display().spanned(span)
            + LrElem::new(TextElem::packed('(') + body + TextElem::packed(')'))
                .pack()
                .spanned(span);

        // Write the value to the output.
        vm.write_one(self.out, out).at(span)?;

        Ok(())
    }
}*/
