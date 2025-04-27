use crate::vm::instructions::{
    DynRef, Emph, Heading, Join, OpsEnumItem, OpsListItem, OpsTermItem, Scoped, Strong,
};
use crate::vm::Readable;

use super::{Compile, CompileTopLevel, Compiler};

use typst_library::diag::{At, SourceResult};
use typst_library::foundations::{IntoValue, Label, NativeElement};
use typst_library::model::{LinkElem, RefElem, Url};
use typst_library::text::{RawContent, RawElem};
use typst_syntax::ast::{self, AstNode};
use typst_utils::PicoStr;

impl Compile for ast::ContentBlock<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let in_math = compiler.in_math;
        compiler.in_math = false;

        let pointer = compiler.insert_pointer();
        let range = compiler.scope(false, |compiler| {
            for expr in self.body().exprs() {
                match expr {
                    ast::Expr::SetRule(set) => {
                        set.compile(compiler)?;
                        compiler.flow();
                    }
                    ast::Expr::ShowRule(show) => {
                        show.compile(compiler)?;
                        compiler.flow();
                    }
                    other => {
                        let value = other.compile(compiler)?;
                        if matches!(value, Readable::Empty | Readable::None) {
                            continue;
                        }

                        compiler.push(Join::new(value, other.span()));
                    }
                }
            }

            Ok(())
        })?;

        compiler.write(pointer, Scoped::new(range, self.span(), true, false));

        compiler.in_math = in_math;
        Ok(Readable::Stack)
    }
}

impl CompileTopLevel for ast::ContentBlock<'_> {
    fn compile_top_level(self, compiler: &mut Compiler) -> SourceResult<()> {
        self.body().compile_top_level(compiler)
    }
}

impl CompileTopLevel for ast::Markup<'_> {
    fn compile_top_level(self, compiler: &mut Compiler) -> SourceResult<()> {
        for expr in self.exprs() {
            match expr {
                ast::Expr::SetRule(set) => {
                    set.compile(compiler)?;
                    compiler.flow();
                }
                ast::Expr::ShowRule(show) => {
                    show.compile(compiler)?;
                    compiler.flow();
                }
                other => {
                    let value = other.compile(compiler)?;
                    if matches!(value, Readable::Empty | Readable::None) {
                        continue;
                    }

                    compiler.push(Join::new(value, other.span()));
                }
            }
        }

        Ok(())
    }
}

impl Compile for ast::Markup<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let in_math = compiler.in_math;
        compiler.in_math = false;

        let pointer = compiler.insert_pointer();
        let range = compiler.enter(|compiler| {
            for expr in self.exprs() {
                match expr {
                    ast::Expr::SetRule(set) => {
                        set.compile(compiler)?;
                        compiler.flow();
                    }
                    ast::Expr::ShowRule(show) => {
                        show.compile(compiler)?;
                        compiler.flow();
                    }
                    other => {
                        let value = other.compile(compiler)?;
                        if matches!(value, Readable::Empty | Readable::None) {
                            continue;
                        }

                        compiler.push(Join::new(value, other.span()));
                    }
                }
            }

            Ok(())
        })?;

        compiler.write(pointer, Scoped::new(range, self.span(), true, false));

        compiler.in_math = in_math;
        Ok(Readable::Stack)
    }
}

impl Compile for ast::Strong<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let body = self.body().compile(compiler)?;

        compiler.push(Strong::new(body, self.span()));

        Ok(Readable::Stack)
    }
}

impl Compile for ast::Emph<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let body = self.body().compile(compiler)?;

        compiler.push(Emph::new(body, self.span()));

        Ok(Readable::Stack)
    }
}

impl Compile for ast::Raw<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let lines = self.lines().map(|line| (line.get().clone(), line.span())).collect();
        let mut elem = RawElem::new(RawContent::Lines(lines)).with_block(self.block());
        if let Some(lang) = self.lang() {
            elem.push_lang(Some(lang.get().clone()));
        }

        Ok(compiler.constant(elem.pack().into_value()))
    }
}

impl Compile for ast::Link<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let url = Url::new(self.get().clone()).at(self.span())?;
        Ok(compiler.constant(LinkElem::from_url(url).pack().into_value()))
    }
}

impl Compile for ast::Ref<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let target = Label::new(PicoStr::intern(self.target()));
        if let Some(supplement) = self.supplement() {
            let supplement = supplement.compile(compiler)?;
            compiler.push(DynRef::new(target, supplement, self.span()));
            Ok(Readable::Stack)
        } else {
            Ok(compiler.constant(RefElem::new(target).pack().into_value()))
        }
    }
}

impl Compile for ast::Heading<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let depth = self.depth();
        let content = self.body().compile(compiler)?;
        compiler.push(Heading::new(depth, content, self.span()));
        Ok(Readable::Stack)
    }
}

impl Compile for ast::ListItem<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let body = self.body().compile(compiler)?;
        compiler.push(OpsListItem::new(body, self.span()));
        Ok(Readable::Stack)
    }
}

impl Compile for ast::EnumItem<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let body = self.body().compile(compiler)?;
        compiler.push(OpsEnumItem::new(body, self.number(), self.span()));
        Ok(Readable::Stack)
    }
}

impl Compile for ast::TermItem<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let term = self.term().compile(compiler)?;
        let desc = self.description().compile(compiler)?;
        compiler.push(OpsTermItem::new(term, desc, self.span()));
        Ok(Readable::Stack)
    }
}
