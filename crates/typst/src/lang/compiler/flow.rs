use typst_syntax::ast::{self, AstNode};

use crate::diag::{bail, SourceResult};
use crate::engine::Engine;
use crate::lang::operands::Readable;

use super::{
    Compile, CompileTopLevel, Compiler, PatternCompile, ReadableGuard, WritableGuard,
};

impl Compile for ast::Conditional<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        // Compile the condition
        let condition = self.condition().compile_to_readable(compiler, engine)?;

        // Create the jump labels
        let if_ = compiler.marker();
        let end = compiler.marker();

        // Create the conditonal jump
        compiler.jump_if(self.condition().span(), condition.clone(), if_);

        // Compile the else body
        if let Some(else_body) = self.else_body() {
            else_body.compile(compiler, engine, output.clone())?;
        } else {
            compiler.copy(self.span(), Readable::none(), output.as_writable());
        }

        // Jump to the end
        compiler.jump(self.span(), end);

        // Compile the if body
        compiler.mark(self.span(), if_);
        self.if_body().compile(compiler, engine, output)?;

        // Mark the end
        compiler.mark(self.span(), end);

        Ok(())
    }
}

impl Compile for ast::WhileLoop<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        compiler.enter_generic(
            engine,
            true,
            |compiler, engine| {
                let mut is_content = false;

                // Create the jump label
                let end = compiler.marker();

                // Compile the condition
                let condition = self.condition().compile_to_readable(compiler, engine)?;

                // Create the conditonal jump
                compiler.jump_if_not(self.condition().span(), condition, end);

                // Mark the beginning of the iteration
                compiler.begin_iter(self.span());

                // Compile the while body
                match self.body() {
                    ast::Expr::Code(code) => {
                        // using `compile_top_level` to avoid double `enter` ops
                        code.body().compile_top_level(compiler, engine)?;
                    }
                    ast::Expr::Content(content) => {
                        is_content = true;

                        // using `compile_top_level` to avoid double `enter` ops
                        content.body().compile_top_level(compiler, engine)?;
                    }
                    other => other.compile(compiler, engine, WritableGuard::Joined)?,
                }
                compiler.flow();

                // Jump to the top
                compiler.jump_top(self.span());

                // Mark the end
                compiler.mark(self.span(), end);

                Ok(is_content)
            },
            |compiler, _, len, is_content| {
                compiler.while_(self.span(), len as u32, is_content, output);
                Ok(())
            },
        )
    }
}

impl Compile for ast::ForLoop<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        compiler.enter_generic(
            engine,
            true,
            |compiler, engine| {
                let mut is_content = false;
                let pattern = self.pattern().compile_pattern(compiler, engine, true)?;
                let pattern_id = compiler.pattern(pattern);
                compiler.next(self.iterable().span(), pattern_id);

                // Mark the beginning of the iteration
                compiler.begin_iter(self.iterable().span());

                match self.body() {
                    ast::Expr::Code(code) => {
                        // using `compile_top_level` to avoid double `enter` ops
                        code.body().compile_top_level(compiler, engine)?;
                    }
                    ast::Expr::Content(content) => {
                        is_content = true;

                        // using `compile_top_level` to avoid double `enter` ops
                        content.body().compile_top_level(compiler, engine)?;
                    }
                    other => other.compile(compiler, engine, WritableGuard::Joined)?,
                }

                compiler.flow();
                compiler.jump_top(self.span());

                Ok(is_content)
            },
            |compiler, engine, len, is_content| {
                let iterable = self.iterable().compile_to_readable(compiler, engine)?;
                compiler.iter(
                    self.iterable().span(),
                    len as u32,
                    iterable,
                    is_content,
                    output,
                );
                Ok(())
            },
        )
    }
}

impl Compile for ast::LoopBreak<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        _: &mut Engine,
        _: WritableGuard,
    ) -> SourceResult<()> {
        if !compiler.in_loop() {
            bail!(self.span(), "cannot break outside of loop");
        }

        compiler.break_(self.span());

        Ok(())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<super::ReadableGuard> {
        self.compile(compiler, engine, WritableGuard::Joined)?;

        Ok(ReadableGuard::None)
    }
}

impl Compile for ast::LoopContinue<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        _: &mut Engine,
        _: WritableGuard,
    ) -> SourceResult<()> {
        if !compiler.in_loop() {
            bail!(self.span(), "cannot continue outside of loop");
        }

        compiler.continue_(self.span());

        Ok(())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<super::ReadableGuard> {
        self.compile(compiler, engine, WritableGuard::Joined)?;

        Ok(ReadableGuard::None)
    }
}

impl Compile for ast::FuncReturn<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        _: WritableGuard,
    ) -> SourceResult<()> {
        if !compiler.in_function() {
            bail!(self.span(), "cannot return outside of function");
        }

        let Some(body) = self.body() else {
            compiler.return_(self.span());
            return Ok(());
        };

        let body = body.compile_to_readable(compiler, engine)?;
        compiler.return_value(self.span(), body);

        Ok(())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<super::ReadableGuard> {
        self.compile(compiler, engine, WritableGuard::Joined)?;

        Ok(ReadableGuard::None)
    }
}
