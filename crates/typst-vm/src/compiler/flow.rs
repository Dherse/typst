use typst_library::diag::{bail, SourceResult};
use typst_syntax::ast::{self, AstNode};
use typst_syntax::{SyntaxKind, SyntaxNode};

use crate::vm::instructions::{
    BreakOp, ContinueOp, Iter, Jump, JumpConditional, Next, ReturnOp, ReturnVal, Scoped, Join, Push
};
use crate::vm::Readable;

use super::pattern::PatternCompile;
use super::{Compile, CompileTopLevel, Compiler, Pointer};

impl Compile for ast::Conditional<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        // Compile the condition.
        // TODO: Add performance optimization bypass for fixed conditions.
        let condition = self.condition().compile(compiler)?;

        // Get a pointer to where the `if` instruction will be
        let top_pointer = compiler.insert_pointer();

        // Compile the if body.
        let if_ = self.if_body().compile(compiler)?;

        // If the output is not the stack, then we should push the value to the stack.
        if !matches!(if_, Readable::Stack) {
            compiler.push(Push::new(if_, self.if_body().span()));
        }

        // If there is an else body compile it too.
        let jmp_pointer = compiler.insert_pointer();
        let end_if = if let Some(else_) = self.else_body() {
            // Prepare the jump pointer.
            let end_if = compiler.here();

            let else_ = else_.compile(compiler)?;

            // If the output is not the stack, then we should push the value to the stack.
            if !matches!(else_, Readable::Stack) {
                compiler.push(Push::new(else_, self.if_body().span()));
            }

            end_if
        } else {
            let end_if = compiler.here();
            compiler.push(Push::new(Readable::None, self.span()));
            end_if
        };

        // Write the if branch jump.
        let end = compiler.here();
        compiler.write(jmp_pointer, Jump::new(end, self.span()));

        compiler.write(
            top_pointer,
            JumpConditional::new(end_if, condition, false, self.condition().span()),
        );

        Ok(Readable::Stack)
    }
}

impl Compile for ast::ForLoop<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let destructuring = matches!(self.pattern(), ast::Pattern::Destructuring(_));
        let iterable = self.iterable().compile(compiler)?;

        let top_pointer = compiler.insert_pointer();
        let next = compiler.here();

        let range = compiler.scope(true, |compiler| {
            let pattern = self.pattern().pattern(compiler, true)?;

            compiler.push(Next::new(pattern.span(), pattern));

            match self.body() {
                ast::Expr::CodeBlock(block) => block.compile_top_level(compiler)?,
                ast::Expr::ContentBlock(block) => block.compile_top_level(compiler)?,
                other => {
                    other.compile(compiler)?;
                }
            }

            compiler.flow();
            compiler.push(Jump::new(next, self.span()));

            Ok(())
        })?;

        compiler.write(
            top_pointer,
            Iter::new(
                iterable,
                range,
                self.iterable().span(),
                self.body().is_display(),
                destructuring.then(|| self.pattern().span()),
            ),
        );

        Ok(Readable::Stack)
    }
}

impl Compile for ast::WhileLoop<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let mut is_content = false;

        let top = compiler.insert_pointer();

        // Check whether the condition is potentially always true.
        let convergent = is_invariant(self.condition().to_untyped())
            && !can_diverge(self.body().to_untyped());

        let mut jump_pointer = Pointer::detached();
        let mut condition = Readable::Empty;
        let scope = compiler.scope(true, |compiler| {
            // The pointer at the top of the loop's body (condition included)
            let top = compiler.here();

            // Compile the condition.
            condition = self.condition().compile(compiler)?;

            // The pointer to the jump instruction we'll write later.
            compiler.flow();
            jump_pointer = compiler.insert_pointer();

            // Compile the while body
            match self.body() {
                ast::Expr::CodeBlock(code) => {
                    // using `compile_top_level` to avoid double `enter` ops
                    code.body().compile_top_level(compiler)?;
                }
                ast::Expr::ContentBlock(content) => {
                    is_content = true;

                    // using `compile_top_level` to avoid double `enter` ops
                    content.body().compile_top_level(compiler)?;
                }
                other => {
                    let out = other.compile(compiler)?;
                    if !matches!(out, Readable::Empty | Readable::None) {
                        compiler.push(Join::new(out, other.span()));
                    }
                }
            }

            // Jump back to the top.
            compiler.flow();
            compiler.push(Jump::new(top, self.span()));

            Ok(())
        })?;

        if condition == Readable::Empty && jump_pointer.is_detached() {
            bail!(self.span(), "scope did not compile"; hint: "this is a compiler bug");
        }

        // Write the jump if false instruction.
        compiler.write(
            jump_pointer,
            JumpConditional::new(scope.end(), condition, false, self.condition().span())
                .with_convergence_check(convergent.then_some(self.condition().span()))
                .with_increment(Some(self.span())),
        );

        // Write the top scope enter instruction.
        compiler.write(top, Scoped::new(scope, self.span(), is_content, true));

        Ok(Readable::Stack)
    }
}

impl Compile for ast::LoopBreak<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        if !compiler.in_loop() {
            bail!(self.span(), "cannot continue outside of loop");
        }

        compiler.push(BreakOp);
        Ok(Readable::Empty)
    }
}

impl Compile for ast::LoopContinue<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        if !compiler.in_loop() {
            bail!(self.span(), "cannot break outside of loop");
        }

        compiler.push(ContinueOp);
        Ok(Readable::Empty)
    }
}

impl Compile for ast::FuncReturn<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        if !compiler.in_function() {
            bail!(self.span(), "cannot return outside of function");
        }

        let Some(body) = self.body() else {
            compiler.push(ReturnOp);
            return Ok(Readable::Empty);
        };

        let body = body.compile(compiler)?;
        compiler.push(ReturnVal::new(body, self.span()));

        Ok(Readable::Empty)
    }
}

/// Whether the expression always evaluates to the same value.
fn is_invariant(expr: &SyntaxNode) -> bool {
    match expr.cast() {
        Some(ast::Expr::Ident(_)) => false,
        Some(ast::Expr::MathIdent(_)) => false,
        Some(ast::Expr::FieldAccess(access)) => {
            is_invariant(access.target().to_untyped())
        }
        Some(ast::Expr::FuncCall(call)) => {
            is_invariant(call.callee().to_untyped())
                && is_invariant(call.args().to_untyped())
        }
        _ => expr.children().all(is_invariant),
    }
}

/// Whether the expression contains a break or return.
fn can_diverge(expr: &SyntaxNode) -> bool {
    matches!(expr.kind(), SyntaxKind::Break | SyntaxKind::Return)
        || expr.children().any(can_diverge)
}
