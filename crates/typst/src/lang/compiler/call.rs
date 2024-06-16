use typst_syntax::ast::{self, AstNode};
use typst_syntax::Span;
use typst_utils::PicoStr;

use crate::engine::Engine;
use crate::foundations::is_mutating_method;
use crate::{diag::SourceResult, lang::operands::Readable};

use super::{Compile, CompileAccess, Compiler, ReadableGuard, WritableGuard};

impl Compile for ast::FuncCall<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        let callee = self.callee();
        let in_math = in_math(callee);

        // Special handling for mutable methods.
        let access = if let ast::Expr::FieldAccess(access) = callee {
            let field = access.field();
            self.access(compiler, engine, is_mutating_method(field.as_str()))?
        } else {
            self.access(compiler, engine, false)?
        };

        let access_id = compiler.access(access.with_math(in_math));
        compiler.call(self.span(), access_id, output);

        Ok(())
    }
}

pub trait ArgsCompile {
    fn compile_args(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        func_call: Span,
    ) -> SourceResult<ReadableGuard>;
}

impl ArgsCompile for ast::Args<'_> {
    fn compile_args(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        func_call: Span,
    ) -> SourceResult<ReadableGuard> {
        let output = compiler.allocate();

        let mut args = self.items();
        let Some(first) = args.next() else {
            compiler.copy(self.span(), Readable::none(), output.clone());

            return Ok(output.into());
        };

        // Allocate the arguments
        let capacity = self.items().count();
        compiler.args(func_call, capacity as u32, output.clone());

        // Compile the first argument
        first.compile(compiler, engine, output.clone().into())?;

        // Compile the rest of the arguments
        for arg in args {
            arg.compile(compiler, engine, output.clone().into())?;
        }

        Ok(output.into())
    }
}

impl Compile for ast::Arg<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        match self {
            ast::Arg::Pos(expr) => {
                let guard = expr.compile_to_readable(compiler, engine)?;
                let span_id = compiler.span(expr.span());
                compiler.push_arg(self.span(), guard, span_id, output);
            }
            ast::Arg::Named(named) => {
                let name = PicoStr::new(named.name().as_str());
                let value = named.expr().compile_to_readable(compiler, engine)?;
                let span_id = compiler.span(named.expr().span());
                compiler.insert_arg(self.span(), name, value, span_id, output);
            }
            ast::Arg::Spread(spread) => {
                let guard = spread.expr().compile_to_readable(compiler, engine)?;
                let span_id = compiler.span(self.span());
                compiler.spread_arg(self.span(), guard, span_id, output);
            }
        }

        Ok(())
    }

    fn compile_to_readable(
        &self,
        _: &mut Compiler<'_>,
        _: &mut Engine,
    ) -> SourceResult<super::ReadableGuard> {
        unreachable!("`Arg` should be compiled through `Compile::compile")
    }
}

/// Checks whether the given expression is in a math context.
fn in_math(expr: ast::Expr) -> bool {
    match expr {
        ast::Expr::MathIdent(_) => true,
        ast::Expr::FieldAccess(access) => in_math(access.target()),
        _ => false,
    }
}
