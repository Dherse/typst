use crate::vm::instructions::{FuncCall, MethodCall, MutMethodCall, Reverse};
use crate::vm::{ArgSegment, Readable};

use super::pattern::AccessCompile;
use super::{Compile, Compiler};

use typst_library::diag::SourceResult;
use typst_syntax::ast::{self, AstNode};
use typst_utils::PicoStr;

impl Compile for ast::FuncCall<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let callee = self.callee();
        let args = self.args();
        let trailing_comma = args.trailing_comma();

        if let ast::Expr::FieldAccess(access) = callee {
            let target = access.target();
            let field = access.field();

            // If it's a mutating method, then we compile an access & a `MutMethodCall`
            if is_mutating_method(&field) {
                let access = target.access(compiler, false)?;
                let arg_vals = args.compile(compiler)?;
                compiler.push(MutMethodCall::new(
                    access,
                    arg_vals,
                    self.span(),
                    field.span(),
                    args.span(),
                    field.get().clone(),
                    compiler.in_math,
                    trailing_comma,
                ));
            } else {
                let access = target.compile(compiler)?;
                let arg_vals = args.compile(compiler)?;
                compiler.push(MethodCall::new(
                    access,
                    arg_vals,
                    self.span(),
                    target.span(),
                    field.span(),
                    args.span(),
                    field.get().clone(),
                    compiler.in_math,
                    trailing_comma,
                ));
            }
        } else if let ast::Expr::Ident(ident) = callee {
            let shadowed = compiler
                .locals
                .borrow_mut()
                .check_std_shadowed(ident.get())
                .then(|| ident.get().clone());

            let arg_vals = args.compile(compiler)?;
            let access = ident.compile(compiler)?;

            compiler.push(FuncCall::new(
                access,
                arg_vals,
                shadowed,
                self.span(),
                ident.span(),
                args.span(),
                compiler.in_math,
                trailing_comma,
            ));
        } else {
            let arg_vals = args.compile(compiler)?;

            if_chain::if_chain!(
                if let ast::Expr::Parenthesized(paren) = callee;
                if let ast::Expr::Ident(ident) = paren.expr();
                then {
                    let shadowed = compiler.locals.borrow_mut().check_std_shadowed(ident.get()).then(|| ident.get().clone());

                    let access = ident.compile(compiler)?;
                    compiler.push(FuncCall::new(
                        access,
                        arg_vals,
                        shadowed,
                        self.span(),
                        ident.span(),
                        args.span(),
                        compiler.in_math,
                        trailing_comma,
                    ));
                }
                else {
                    let access = callee.compile(compiler)?;
                    compiler.push(FuncCall::new(
                        access,
                        arg_vals,
                        None,
                        self.span(),
                        callee.span(),
                        args.span(),
                        compiler.in_math,
                        trailing_comma,
                    ));
                }
            );
        }

        Ok(Readable::Stack)
    }
}

pub(crate) trait ArgsExt {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Vec<ArgSegment>>;
}

impl ArgsExt for ast::Args<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Vec<ArgSegment>> {
        let mut args = Vec::with_capacity(self.items().count());

        // We cannot reverse although it would make sense because of eval order!
        let mut stack_len: usize = 0;
        for arg in self.items() {
            match arg {
                ast::Arg::Pos(expr) => {
                    let readable = expr.compile(compiler)?;
                    stack_len += matches!(readable, Readable::Stack) as usize;

                    args.push(ArgSegment::Single(arg.span(), expr.span(), readable));
                }
                ast::Arg::Named(named) => {
                    let readable = named.expr().compile(compiler)?;
                    stack_len += matches!(readable, Readable::Stack) as usize;

                    args.push(ArgSegment::Named(
                        arg.span(),
                        named.expr().span(),
                        PicoStr::intern(named.name().get()),
                        readable,
                    ));
                }
                ast::Arg::Spread(spread) => {
                    let readable = spread.expr().compile(compiler)?;
                    stack_len += matches!(readable, Readable::Stack) as usize;

                    args.push(ArgSegment::Spread(spread.span(), readable));
                }
            }
        }

        if stack_len > 1 {
            compiler.push(Reverse::new(stack_len, self.span()))
        }

        Ok(args)
    }
}

/// Whether a specific method is mutating.
pub(crate) fn is_mutating_method(method: &str) -> bool {
    matches!(method, "push" | "pop" | "insert" | "remove")
}
