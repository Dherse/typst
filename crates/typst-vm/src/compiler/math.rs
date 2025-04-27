use crate::vm::instructions::{Attach, Delimited, Equation, Frac, Join, Root, Scoped};
use crate::vm::Readable;

use super::{Compile, Compiler};

use ecow::eco_format;
use typst_library::diag::SourceResult;
use typst_library::foundations::{IntoValue, NativeElement, Symbol, SymbolElem, Value};
use typst_library::math::{AlignPointElem, PrimesElem};
use typst_library::text::TextElem;
use typst_syntax::ast::{self, AstNode, MathTextKind};
use typst_syntax::Span;

impl Compile for ast::Equation<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let in_math = compiler.in_math;

        compiler.in_math = true;
        let body = self.body().compile(compiler)?;
        compiler.push(Equation::new(body, self.block(), self.span()));
        compiler.in_math = in_math;

        Ok(Readable::Stack)
    }
}

impl Compile for ast::Math<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let pointer = compiler.insert_pointer();
        let range = compiler.enter(|compiler| {
            let in_math = compiler.in_math;
            compiler.in_math = true;
            for expr in self.exprs() {
                let value = expr.compile(compiler)?;
                if !matches!(value, Readable::Empty | Readable::None) {
                    compiler.push(Join::new(value, self.span()));
                }
            }
            compiler.in_math = in_math;

            Ok(())
        })?;

        compiler.write(pointer, Scoped::new(range, self.span(), true, false));

        Ok(Readable::Stack)
    }
}

impl Compile for ast::MathText<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        match self.get() {
            MathTextKind::Character(c) => {
                Ok(compiler.constant(SymbolElem::packed(c).into_value()))
            }
            MathTextKind::Number(text) => {
                Ok(compiler.constant(TextElem::packed(text.clone()).into_value()))
            }
        }
    }
}

impl Compile for ast::MathIdent<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        compiler.get_math(self.get(), self.span())
    }
}

impl Compile for ast::MathShorthand<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        Ok(compiler.constant(Value::Symbol(Symbol::single(self.get()))))
    }
}

impl Compile for ast::MathAlignPoint<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        Ok(compiler.constant(AlignPointElem::shared().clone().into_value()))
    }
}

impl Compile for ast::MathDelimited<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let open = self.open().compile(compiler)?;
        let body = self.body().compile(compiler)?;
        let close = self.close().compile(compiler)?;

        compiler.push(Delimited::new(
            open,
            self.open().span(),
            body,
            self.body().span(),
            close,
            self.close().span(),
        ));

        Ok(Readable::Stack)
    }
}

impl Compile for ast::MathAttach<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let base = self.base().compile(compiler)?;

        let mut top = None;
        let mut primes = None;
        if let Some(t) = self.top().map(|value| value.compile(compiler)).transpose()? {
            top = Some(t);
        }

        if let Some(p) = self.primes().map(|value| value.compile(compiler)).transpose()? {
            primes = Some(p);
        };

        let bottom = self
            .bottom()
            .map_or(Ok(None), |value| value.compile(compiler).map(Some))?;

        compiler.push(Attach::new(
            base,
            self.base().span(),
            top,
            self.top().map_or_else(Span::detached, |t| t.span()),
            primes,
            self.primes().map_or_else(Span::detached, |t| t.span()),
            bottom,
            self.bottom().map_or_else(Span::detached, |t| t.span()),
            self.span(),
        ));

        Ok(Readable::Stack)
    }
}

impl Compile for ast::MathPrimes<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        Ok(compiler.constant(PrimesElem::new(self.count()).pack().into_value()))
    }
}

impl Compile for ast::MathFrac<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let num = self.num().compile(compiler)?;
        let denom = self.denom().compile(compiler)?;

        compiler.push(Frac::new(num, self.num().span(), denom, self.denom().span()));

        Ok(Readable::Stack)
    }
}

impl Compile for ast::MathRoot<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let radicand = self.radicand().compile(compiler)?;
        let degree = self
            .index()
            .map(|i| TextElem::packed(eco_format!("{i}")).spanned(self.span()))
            .map(|v| compiler.constant(v.into_value()));

        compiler.push(Root::new(radicand, self.radicand().span(), degree, self.span()));

        Ok(Readable::Stack)
    }
}
