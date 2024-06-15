use ecow::eco_format;
use typst_syntax::ast::{self, AstNode};

use crate::diag::{At, SourceResult};
use crate::engine::Engine;
use crate::foundations::{unknown_variable, NativeElement};
use crate::math::{AlignPointElem, PrimesElem};
use crate::text::TextElem;

use super::{
    copy_constant, Compile, CompileTopLevel, Compiler, ReadableGuard, WritableGuard,
};

impl CompileTopLevel for ast::Math<'_> {
    fn compile_top_level(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<()> {
        for expr in self.exprs() {
            expr.compile(compiler, engine, WritableGuard::Joined)?;
            compiler.flow();
        }

        Ok(())
    }
}

impl Compile for ast::Math<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        compiler.enter(engine, self.span(), output, |compiler, engine| {
            for expr in self.exprs() {
                expr.compile(compiler, engine, WritableGuard::Joined)?;
                compiler.flow();
            }

            Ok(true)
        })
    }
}

impl Compile for ast::MathIdent<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        let read = self.compile_to_readable(compiler, engine)?;

        compiler.copy(self.span(), read, output);

        Ok(())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        _: &mut Engine,
    ) -> SourceResult<ReadableGuard> {
        let Some(value) = compiler.read_math(self.span(), self.get().as_str()) else {
            return Err(unknown_variable(self.as_str())).at(self.span());
        };

        Ok(value)
    }
}

impl Compile for ast::MathAlignPoint<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        copy_constant!(self, compiler, engine, output);
        Ok(())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        _: &mut Engine,
    ) -> SourceResult<ReadableGuard> {
        let value = AlignPointElem::new().pack();
        Ok(compiler.const_(value).into())
    }
}

impl Compile for ast::MathDelimited<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        let left = self.open().compile_to_readable(compiler, engine)?;
        let body = self.body().compile_to_readable(compiler, engine)?;
        let right = self.close().compile_to_readable(compiler, engine)?;

        compiler.delimited(self.span(), left, body, right, output);

        Ok(())
    }
}

impl Compile for ast::MathAttach<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        let base = self.base().compile_to_readable(compiler, engine)?;

        let mut t = None;
        let mut tr = None;
        if let Some(top) = self
            .top()
            .map(|value| value.compile_to_readable(compiler, engine))
            .transpose()?
        {
            t = Some(top);
        } else if let Some(primes) = self
            .primes()
            .map(|value| value.compile_to_readable(compiler, engine))
            .transpose()?
        {
            tr = Some(primes);
        };

        let b = self.bottom().map_or(Ok(None), |value| {
            value.compile_to_readable(compiler, engine).map(Some)
        })?;

        compiler.attach(
            self.span(),
            base,
            t.map(|b| b.into()),
            tr.map(|b| b.into()),
            b.map(|b| b.into()),
            output,
        );

        Ok(())
    }
}

impl Compile for ast::MathPrimes<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        copy_constant!(self, compiler, engine, output);
        Ok(())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        _: &mut Engine,
    ) -> SourceResult<ReadableGuard> {
        let primes = PrimesElem::new(self.count()).pack().spanned(self.span());
        Ok(compiler.const_(primes).into())
    }
}

impl Compile for ast::MathFrac<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        let num = self.num().compile_to_readable(compiler, engine)?;
        let den = self.denom().compile_to_readable(compiler, engine)?;

        compiler.frac(self.span(), num, den, output);

        Ok(())
    }
}

impl Compile for ast::MathRoot<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        let radicand = self.radicand().compile_to_readable(compiler, engine)?;
        let degree = self
            .index()
            .map(|i| TextElem::packed(eco_format!("{i}")).spanned(self.span()))
            .map(|v| compiler.const_(v));

        compiler.root(self.span(), degree.map(Into::into), radicand, output);

        Ok(())
    }
}
