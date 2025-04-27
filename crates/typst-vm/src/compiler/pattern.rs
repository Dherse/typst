use typst_library::diag::{bail, SourceResult};
use typst_syntax::ast::{self, AstNode};

use crate::vm::{AccessSegment, MutAccess, Pattern, PatternItem, Readable};

use super::call::ArgsExt;
use super::Compiler;

pub trait AccessCompile {
    fn access(&self, compiler: &mut Compiler, declare: bool) -> SourceResult<MutAccess>;
}

impl AccessCompile for ast::Expr<'_> {
    fn access(&self, compiler: &mut Compiler, declare: bool) -> SourceResult<MutAccess> {
        match self {
            Self::Ident(v) => v.access(compiler, declare),
            Self::Parenthesized(v) => v.access(compiler, declare),
            Self::FieldAccess(v) => v.access(compiler, declare),
            Self::FuncCall(v) => v.access(compiler, declare),
            _ => {
                bail!(self.span(), "cannot mutate a temporary value");
            }
        }
    }
}

impl AccessCompile for ast::Ident<'_> {
    fn access(&self, compiler: &mut Compiler, declare: bool) -> SourceResult<MutAccess> {
        let Readable::Slot(head) =
            compiler.get_or_declare(self.get(), self.span(), declare)?
        else {
            bail!(
                self.span(),
                "cannot mutate a constant: {}", self.as_str();
                hint: "you can shadow the constant with `let {0} = {0}`", self.as_str()
            )
        };

        Ok(MutAccess {
            head,
            head_name: self.get().clone(),
            segments: vec![],
            span: self.span(),
        })
    }
}

impl AccessCompile for ast::Parenthesized<'_> {
    fn access(&self, compiler: &mut Compiler, declare: bool) -> SourceResult<MutAccess> {
        self.expr().access(compiler, declare)
    }
}

impl AccessCompile for ast::FieldAccess<'_> {
    fn access(&self, compiler: &mut Compiler, declare: bool) -> SourceResult<MutAccess> {
        let mut parent = self.target().access(compiler, declare)?;

        parent.segments.push(AccessSegment::Field {
            name: self.field().get().clone(),
            span: self.field().span(),
        });

        Ok(parent)
    }
}

impl AccessCompile for ast::FuncCall<'_> {
    fn access(&self, compiler: &mut Compiler, declare: bool) -> SourceResult<MutAccess> {
        let ast::Expr::FieldAccess(access) = self.callee() else {
            bail!(self.span(), "cannot mutate a temporary value");
        };

        let method = access.field();
        let mut parent = access.target().access(compiler, declare)?;

        let args = self.args().compile(compiler)?;

        parent.segments.push(AccessSegment::Call {
            method: method.get().clone(),
            args,
            span: self.span(),
        });

        Ok(parent)
    }
}

pub trait PatternCompile {
    fn pattern(&self, compiler: &mut Compiler, declare: bool) -> SourceResult<Pattern>;
}

impl PatternCompile for ast::Pattern<'_> {
    fn pattern(&self, compiler: &mut Compiler, declare: bool) -> SourceResult<Pattern> {
        Ok(match self {
            ast::Pattern::Normal(expr) => {
                Pattern::Single(expr.access(compiler, declare)?)
            }
            ast::Pattern::Placeholder(underscore) => {
                Pattern::Placeholder(underscore.span())
            }
            ast::Pattern::Parenthesized(parenthesized) => {
                parenthesized.pattern().pattern(compiler, declare)?
            }
            ast::Pattern::Destructuring(destructuring) => {
                let mut items = Vec::with_capacity(destructuring.items().count());
                for item in destructuring.items() {
                    match item {
                        ast::DestructuringItem::Pattern(pattern) => {
                            let pat = pattern.pattern(compiler, declare)?;
                            match pat {
                                Pattern::Single(access) => {
                                    items.push(PatternItem::Single(access))
                                }
                                Pattern::Placeholder(span) => {
                                    items.push(PatternItem::Placeholder(span));
                                }
                                other => items.push(PatternItem::Nested(Box::new(other))),
                            }
                        }
                        ast::DestructuringItem::Named(named) => {
                            let access = named.pattern().pattern(compiler, declare)?;
                            match access {
                                Pattern::Single(access) => {
                                    items.push(PatternItem::Named(
                                        access,
                                        named.name().get().clone().into(),
                                    ))
                                }
                                Pattern::Placeholder(span) => {
                                    items.push(PatternItem::PlaceholderNamed(
                                        span,
                                        named.name().get().clone().into(),
                                    ))
                                }
                                nested @ Pattern::Items(_, _) => {
                                    items.push(PatternItem::NestedNamed(
                                        Box::new(nested),
                                        named.name().get().clone().into(),
                                    ))
                                }
                            }
                        }
                        ast::DestructuringItem::Spread(spread) => {
                            let Some(expr) = spread.sink_expr() else {
                                items.push(PatternItem::SpreadDiscard(spread.span()));
                                continue;
                            };

                            let access = expr.access(compiler, declare)?;
                            items.push(PatternItem::Spread(access))
                        }
                    }
                }

                Pattern::Items(items, destructuring.span())
            }
        })
    }
}
