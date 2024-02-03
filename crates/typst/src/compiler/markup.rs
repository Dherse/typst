use std::num::NonZeroU32;

use typst_syntax::ast::{self, AstNode};

use crate::diag::{bail, SourceResult};
use crate::engine::Engine;
use crate::foundations::{IntoValue, Label, NativeElement, Value};
use crate::model::{LinkElem, ParbreakElem};
use crate::symbols::Symbol;
use crate::text::{LinebreakElem, RawElem, SmartQuoteElem};
use crate::text::{SpaceElem, TextElem};
use crate::vm::{Constant, OptionalReadable};

use super::{Compile, CompileTopLevel, Compiler, ReadableGuard, WritableGuard};

impl CompileTopLevel for ast::Markup<'_> {
    fn compile_top_level(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<()> {
        for expr in self.exprs() {
            // Handle set rules specially.
            if let ast::Expr::Set(set) = expr {
                let style = set.compile(engine, compiler)?;
                compiler.styled(set.span(), &style);
                compiler.flow();
                continue;
            }

            // Handle show rules specially.
            if let ast::Expr::Show(show) = expr {
                let style = show.compile(engine, compiler)?;
                compiler.styled(show.span(), &style);
                compiler.flow();
                continue;
            }

            // Compile the expression, appending its output to the join
            // output.
            expr.compile_into(engine, compiler, Some(WritableGuard::Joined))?;
            compiler.flow();
        }

        Ok(())
    }
}

impl Compile for ast::Markup<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        compiler.enter(
            engine,
            self.span(),
            false,
            output.as_ref().map(|w| w.as_writable()),
            true,
            |compiler, engine, _| {
                let join_output = output.is_some().then(|| WritableGuard::Joined);
                for expr in self.exprs() {
                    // Handle set rules specially.
                    if let ast::Expr::Set(set) = expr {
                        let style = set.compile(engine, compiler)?;
                        if join_output.is_none() {
                            bail!(set.span(), "cannot set style without output");
                        }

                        compiler.styled(set.span(), &style);
                        compiler.flow();
                        continue;
                    }

                    // Handle show rules specially.
                    if let ast::Expr::Show(show) = expr {
                        let style = show.compile(engine, compiler)?;
                        if join_output.is_none() {
                            bail!(show.span(), "cannot set style without output");
                        }

                        compiler.styled(show.span(), &style);
                        compiler.flow();
                        continue;
                    }

                    // Compile the expression, appending its output to the join
                    // output.
                    expr.compile_into(engine, compiler, join_output.clone())?;
                    compiler.flow();
                }

                Ok(())
            },
        )
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        // Get an output register.
        let reg = compiler.register();

        // Compile into the register.
        let output = Some(WritableGuard::from(reg.clone()));
        self.compile_into(engine, compiler, output)?;

        // Return the register.
        Ok(reg.into())
    }
}

impl Compile for ast::Text<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), &const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = TextElem::new(self.get().clone()).pack().spanned(self.span());
        let value = compiler.const_(value.into_value());

        Ok(value.into())
    }
}

impl Compile for ast::Space<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), &const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = SpaceElem::new().pack();
        let value = compiler.const_(value.into_value());

        Ok(value.into())
    }
}

impl Compile for ast::Linebreak<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), &const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = LinebreakElem::new().pack();
        let value = compiler.const_(value.into_value());

        Ok(value.into())
    }
}

impl Compile for ast::Parbreak<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), &const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = ParbreakElem::new().pack();
        let value = compiler.const_(value.into_value());

        Ok(value.into())
    }
}

impl Compile for ast::Escape<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), &const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = Value::Symbol(Symbol::single(self.get()));
        let value = compiler.const_(value.into_value());

        Ok(value.into())
    }
}

impl Compile for ast::Shorthand<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), &const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = Value::Symbol(Symbol::single(self.get()));
        let value = compiler.const_(value.into_value());

        Ok(value.into())
    }
}

impl Compile for ast::SmartQuote<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), &const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = SmartQuoteElem::new()
            .with_double(self.double())
            .pack()
            .spanned(self.span());
        Ok(compiler.const_(value.into_value()).into())
    }
}

impl Compile for ast::Strong<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let body = self.body().compile(engine, compiler)?;
            compiler.strong(self.span(), &body, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let out = compiler.register();
        self.compile_into(engine, compiler, Some(out.clone().into()))?;
        Ok(out.into())
    }
}

impl Compile for ast::Emph<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let body = self.body().compile(engine, compiler)?;
            compiler.emph(self.span(), &body, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let out = compiler.register();
        self.compile_into(engine, compiler, Some(out.clone().into()))?;
        Ok(out.into())
    }
}

impl Compile for ast::Raw<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = Constant;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let mut elem = RawElem::new(self.text()).with_block(self.block());
        if let Some(lang) = self.lang() {
            elem.push_lang(Some(lang.into()));
        }

        Ok(compiler.const_(elem.pack().spanned(self.span()).into_value()))
    }
}

impl Compile for ast::Link<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = Constant;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = LinkElem::from_url(self.get().clone()).pack().spanned(self.span());

        Ok(compiler.const_(value.into_value()))
    }
}

impl Compile for ast::Label<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = Constant;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let const_ = self.compile(engine, compiler)?;

            compiler.copy(self.span(), const_, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        _: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let value = Value::Label(Label::new(self.get()));

        Ok(compiler.const_(value.into_value()))
    }
}

impl Compile for ast::Ref<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let label = compiler.label(self.target());
            let supplement = self
                .supplement()
                .map(|sup| sup.compile(engine, compiler))
                .transpose()?;

            compiler.ref_(
                self.span(),
                label,
                supplement
                    .map(|r| OptionalReadable::some(r.as_readable()))
                    .unwrap_or_else(OptionalReadable::none),
                &output,
            );
        }

        Ok(())
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let output = compiler.register();
        self.compile_into(engine, compiler, Some(output.clone().into()))?;
        Ok(output.into())
    }
}

impl Compile for ast::Heading<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let level = self.level();
            let body = self.body().compile(engine, compiler)?;

            compiler.heading(self.span(), &body, level.get() as u32, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let output = compiler.register();
        self.compile_into(engine, compiler, Some(output.clone().into()))?;
        Ok(output.into())
    }
}

impl Compile for ast::ListItem<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let body = self.body().compile(engine, compiler)?;
            compiler.list_item(self.span(), &body, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let output = compiler.register();
        self.compile_into(engine, compiler, Some(output.clone().into()))?;
        Ok(output.into())
    }
}

impl Compile for ast::EnumItem<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let number = self.number().and_then(|n| NonZeroU32::new(n as u32 + 1));
            let body = self.body().compile(engine, compiler)?;
            compiler.enum_item(self.span(), &body, number, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let output = compiler.register();
        self.compile_into(engine, compiler, Some(output.clone().into()))?;
        Ok(output.into())
    }
}

impl Compile for ast::TermItem<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let term = self.term().compile(engine, compiler)?;
            let description = self.description().compile(engine, compiler)?;
            compiler.term_item(self.span(), &term, &description, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let output = compiler.register();
        self.compile_into(engine, compiler, Some(output.clone().into()))?;
        Ok(output.into())
    }
}

impl Compile for ast::Equation<'_> {
    type Output = Option<WritableGuard>;
    type IntoOutput = ReadableGuard;

    fn compile_into(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
        output: Self::Output,
    ) -> SourceResult<()> {
        if let Some(output) = output {
            let body = self.body().compile(engine, compiler)?;
            compiler.equation(self.span(), &body, &output);
        }

        Ok(())
    }

    fn compile(
        &self,
        engine: &mut Engine,
        compiler: &mut Compiler,
    ) -> SourceResult<Self::IntoOutput> {
        let output = compiler.register();
        self.compile_into(engine, compiler, Some(output.clone().into()))?;
        Ok(output.into())
    }
}