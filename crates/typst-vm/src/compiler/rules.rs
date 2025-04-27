use typst_library::diag::SourceResult;
use typst_syntax::ast::{self, AstNode};
use typst_syntax::Span;

use crate::vm::instructions::{JumpConditional, Set, Show, ShowSet};
use crate::vm::{ArgSegment, Readable};

use super::call::ArgsExt;
use super::{Compile, Compiler, Pointer};

impl Compile for ast::SetRule<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let (target, args, else_) = compile_set(&self, compiler)?;

        compiler.push(Set::new(target, args, self.span()));
        if let Some((condition, else_)) = else_ {
            compiler.write(
                else_,
                JumpConditional::new(
                    compiler.here(),
                    condition,
                    false,
                    self.condition().map_or(self.span(), |c| c.span()),
                ),
            );
        }

        Ok(Readable::Empty)
    }
}

impl Compile for ast::ShowRule<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let selector = self.selector().map(|sel| sel.compile(compiler)).transpose()?;

        let transform = self.transform();
        match transform {
            ast::Expr::SetRule(set) => {
                let (target, args, else_) = compile_set(&set, compiler)?;
                compiler.push(ShowSet::new(
                    selector.map(|s| s.into()),
                    self.selector().map_or_else(Span::detached, |s| s.span()),
                    target,
                    args,
                    transform.span(),
                ));

                if let Some((condition, else_)) = else_ {
                    compiler.write(
                        else_,
                        JumpConditional::new(
                            compiler.here(),
                            condition,
                            false,
                            set.condition().map_or(self.span(), |c| c.span()),
                        ),
                    );
                }
            }
            other => {
                let other = other.compile(compiler)?;
                compiler.push(Show::new(
                    selector.map(|s| s.into()),
                    self.selector().map_or_else(Span::detached, |s| s.span()),
                    other,
                    transform.span(),
                ));
            }
        }

        Ok(Readable::Empty)
    }
}

fn compile_set(
    set: &ast::SetRule<'_>,
    compiler: &mut super::Compiler,
) -> SourceResult<(Readable, Vec<ArgSegment>, Option<(Readable, Pointer)>)> {
    if let Some(expr) = set.condition() {
        // Compile the condition.
        let condition = expr.compile(compiler)?;

        // Create the jump marker.
        let else_ = compiler.insert_pointer();

        // Compile the set.
        let target = set.target().compile(compiler)?;
        let args = set.args().compile(compiler)?;

        Ok((target, args, Some((condition, else_))))
    } else {
        let target = set.target().compile(compiler)?;
        let args = set.args().compile(compiler)?;

        Ok((target, args, None))
    }
}
