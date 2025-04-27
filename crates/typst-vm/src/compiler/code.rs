use crate::compiler::pattern::AccessCompile;
use crate::vm::instructions::{
    Add, AddAssign, AllocateArray, AllocateDict, And, ArrayPush, ArraySpread, Assign,
    AssignOp, BinaryOp, Destructure, DictInsert, DictInsertKeyed, DictSpread, Div,
    DivAssign, Duplicate, Eq, FieldAccess, Gt, Gte, In, Join, Jump, JumpConditional, Lt,
    Lte, Mul, MulAssign, Neg, Neq, Not, NotIn, Or, Pos, Push, Scoped, Sub, SubAssign,
    Write,
};
use crate::vm::Readable;

use super::func::compile_closure;
use super::pattern::PatternCompile;
use super::{Compile, CompileTopLevel, Compiler};

use typst_library::diag::{bail, error, SourceResult};
use typst_library::foundations::{IntoValue, Label, NativeElement, Str, Symbol, Value};
use typst_library::model::ParbreakElem;
use typst_library::text::{LinebreakElem, SmartQuoteElem, SpaceElem, TextElem};
use typst_syntax::ast::{self, AstNode};
use typst_utils::{PicoStr, Scalar};

impl Compile for ast::CodeBlock<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let in_math = compiler.in_math;
        compiler.in_math = false;

        let pointer = compiler.insert_pointer();
        let range = compiler
            .scope(false, |compiler| compile_code(compiler, &mut self.body().exprs()))?;

        compiler.write(pointer, Scoped::new(range, self.span(), false, false));

        compiler.in_math = in_math;
        Ok(Readable::Stack)
    }
}

impl CompileTopLevel for ast::CodeBlock<'_> {
    fn compile_top_level(self, compiler: &mut Compiler) -> SourceResult<()> {
        self.body().compile_top_level(compiler)
    }
}

impl Compile for ast::Code<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let in_math = compiler.in_math;
        compiler.in_math = false;

        let pointer = compiler.insert_pointer();
        let range =
            compiler.enter(|compiler| compile_code(compiler, &mut self.exprs()))?;

        compiler.write(pointer, Scoped::new(range, self.span(), false, false));

        compiler.in_math = in_math;
        Ok(Readable::Stack)
    }
}

impl CompileTopLevel for ast::Code<'_> {
    fn compile_top_level(self, compiler: &mut Compiler) -> SourceResult<()> {
        compile_code(compiler, &mut self.exprs())
    }
}

/// Compile a stream of expressions.
fn compile_code<'a>(
    compiler: &mut Compiler,
    exprs: &mut impl Iterator<Item = ast::Expr<'a>>,
) -> SourceResult<()> {
    while let Some(expr) = exprs.next() {
        match expr {
            ast::Expr::SetRule(set) => {
                set.compile(compiler)?;
                compiler.flow();
            }
            ast::Expr::ShowRule(show) => {
                show.compile(compiler)?;
                compiler.flow();
            }
            _ => {
                let out = expr.compile(compiler)?;
                if !matches!(out, Readable::Empty | Readable::None) {
                    compiler.push(Join::new(out, expr.span()));
                }
            }
        };
    }

    Ok(())
}

impl Compile for ast::Expr<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let span = self.span();
        let forbidden = |name| {
            error!(span, "{} is only allowed directly in code and content blocks", name)
        };

        match self {
            Self::Text(text) => text.compile(compiler),
            Self::Space(space) => space.compile(compiler),
            Self::Linebreak(linebreak) => linebreak.compile(compiler),
            Self::Parbreak(parbreak) => parbreak.compile(compiler),
            Self::Escape(escape) => escape.compile(compiler),
            Self::Shorthand(shorthand) => shorthand.compile(compiler),
            Self::SmartQuote(smart_quote) => smart_quote.compile(compiler),
            Self::Strong(strong) => strong.compile(compiler),
            Self::Emph(emph) => emph.compile(compiler),
            Self::Raw(raw) => raw.compile(compiler),
            Self::Link(link) => link.compile(compiler),
            Self::Label(label) => label.compile(compiler),
            Self::Ref(ref_) => ref_.compile(compiler),
            Self::Heading(heading) => heading.compile(compiler),
            Self::ListItem(list_item) => list_item.compile(compiler),
            Self::EnumItem(enum_item) => enum_item.compile(compiler),
            Self::TermItem(term_item) => term_item.compile(compiler),
            Self::Equation(equation) => equation.compile(compiler),
            Self::Math(math) => math.compile(compiler),
            Self::MathText(math_text) => math_text.compile(compiler),
            Self::MathIdent(math_ident) => math_ident.compile(compiler),
            Self::MathShorthand(math_shorthand) => math_shorthand.compile(compiler),
            Self::MathAlignPoint(math_align_point) => math_align_point.compile(compiler),
            Self::MathDelimited(math_delimited) => math_delimited.compile(compiler),
            Self::MathAttach(math_attach) => math_attach.compile(compiler),
            Self::MathPrimes(math_primes) => math_primes.compile(compiler),
            Self::MathFrac(math_frac) => math_frac.compile(compiler),
            Self::MathRoot(math_root) => math_root.compile(compiler),
            Self::Ident(ident) => ident.compile(compiler),
            Self::None(_) => Ok(Readable::None),
            Self::Auto(_) => Ok(Readable::Auto),
            Self::Bool(bool) => Ok(Readable::Bool(bool.get())),
            Self::Int(int) => Ok(Readable::Int(int.get())),
            Self::Float(float) => Ok(Readable::Float(Scalar::new(float.get()))),
            Self::Numeric(numeric) => {
                Ok(add_constant(compiler, Value::numeric(numeric.get())))
            }
            Self::Str(str) => Ok(add_constant(compiler, str.get())),
            Self::CodeBlock(code_block) => code_block.compile(compiler),
            Self::ContentBlock(content_block) => content_block.compile(compiler),
            Self::Parenthesized(parenthesized) => parenthesized.expr().compile(compiler),
            Self::Array(array) => array.compile(compiler),
            Self::Dict(dict) => dict.compile(compiler),
            Self::Unary(unary) => unary.compile(compiler),
            Self::Binary(binary) => binary.compile(compiler),
            Self::FieldAccess(field_access) => field_access.compile(compiler),
            Self::FuncCall(func_call) => func_call.compile(compiler),
            Self::Closure(closure) => closure.compile(compiler),
            Self::LetBinding(let_binding) => let_binding.compile(compiler),
            Self::DestructAssignment(destruct_assignment) => {
                destruct_assignment.compile(compiler)
            }
            Self::SetRule(_) => bail!(forbidden("set")),
            Self::ShowRule(_) => bail!(forbidden("show")),
            Self::Contextual(contextual) => contextual.compile(compiler),
            Self::Conditional(conditional) => conditional.compile(compiler),
            Self::WhileLoop(while_loop) => while_loop.compile(compiler),
            Self::ForLoop(for_loop) => for_loop.compile(compiler),
            Self::ModuleImport(module_import) => module_import.compile(compiler),
            Self::ModuleInclude(module_include) => module_include.compile(compiler),
            Self::LoopBreak(loop_break) => loop_break.compile(compiler),
            Self::LoopContinue(loop_continue) => loop_continue.compile(compiler),
            Self::FuncReturn(func_return) => func_return.compile(compiler),
        }
    }
}

impl Compile for ast::Ident<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        compiler.get(self.get(), self.span())
    }
}

impl Compile for ast::Array<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        compiler.push(AllocateArray::new(self.items().count()));

        for item in self.items() {
            match item {
                ast::ArrayItem::Pos(expr) => {
                    let value = expr.compile(compiler)?;
                    compiler.push(ArrayPush::new(value, expr.span()));
                }
                ast::ArrayItem::Spread(spread) => {
                    let value = spread.expr().compile(compiler)?;
                    compiler.push(ArraySpread::new(value, spread.span()));
                }
            }
        }

        Ok(Readable::Stack)
    }
}

impl Compile for ast::Dict<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        compiler.push(AllocateDict::new(self.items().count()));

        for item in self.items() {
            match item {
                ast::DictItem::Named(named) => {
                    let value = named.expr().compile(compiler)?;
                    compiler.push(DictInsert::new(
                        named.name().get().clone().into(),
                        value,
                        named.span(),
                    ));
                }
                ast::DictItem::Keyed(named) => {
                    let key = named.key().compile(compiler)?;
                    let value = named.expr().compile(compiler)?;
                    compiler.push(DictInsertKeyed::new(
                        key,
                        named.key().span(),
                        value,
                        named.span(),
                    ));
                }
                ast::DictItem::Spread(spread) => {
                    let value = spread.expr().compile(compiler)?;
                    compiler.push(DictSpread::new(value, spread.span()));
                }
            }
        }

        Ok(Readable::Stack)
    }
}

impl Compile for ast::Unary<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let rhs = self.expr().compile(compiler)?;
        match self.op() {
            ast::UnOp::Pos => compiler.push(Pos::new(rhs).at(self.span())),
            ast::UnOp::Neg => compiler.push(Neg::new(rhs).at(self.span())),
            ast::UnOp::Not => compiler.push(Not::new(rhs).at(self.span())),
        }

        Ok(Readable::Stack)
    }
}

impl Compile for ast::Binary<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        match self.op() {
            ast::BinOp::Add => apply_binary::<Add>(self, compiler),
            ast::BinOp::Sub => apply_binary::<Sub>(self, compiler),
            ast::BinOp::Mul => apply_binary::<Mul>(self, compiler),
            ast::BinOp::Div => apply_binary::<Div>(self, compiler),
            ast::BinOp::And => apply_binary_shortcircuit::<And>(self, true, compiler),
            ast::BinOp::Or => apply_binary_shortcircuit::<Or>(self, false, compiler),
            ast::BinOp::Eq => apply_binary::<Eq>(self, compiler),
            ast::BinOp::Neq => apply_binary::<Neq>(self, compiler),
            ast::BinOp::Lt => apply_binary::<Lt>(self, compiler),
            ast::BinOp::Leq => apply_binary::<Lte>(self, compiler),
            ast::BinOp::Gt => apply_binary::<Gt>(self, compiler),
            ast::BinOp::Geq => apply_binary::<Gte>(self, compiler),
            ast::BinOp::In => apply_binary::<In>(self, compiler),
            ast::BinOp::NotIn => apply_binary::<NotIn>(self, compiler),
            ast::BinOp::Assign => apply_assignment::<Assign>(self, compiler),
            ast::BinOp::AddAssign => apply_assignment::<AddAssign>(self, compiler),
            ast::BinOp::SubAssign => apply_assignment::<SubAssign>(self, compiler),
            ast::BinOp::MulAssign => apply_assignment::<MulAssign>(self, compiler),
            ast::BinOp::DivAssign => apply_assignment::<DivAssign>(self, compiler),
        }
    }
}

fn apply_binary<Op: BinaryOp>(
    bin: ast::Binary<'_>,
    compiler: &mut Compiler,
) -> SourceResult<Readable> {
    let lhs = bin.lhs().compile(compiler)?;
    let rhs = bin.rhs().compile(compiler)?;

    // Optimize none additions.
    match (lhs, rhs) {
        (Readable::Empty, val) | (val, Readable::Empty) => Ok(val),
        (lhs, rhs) => {
            compiler.push(Op::create(lhs, rhs, bin.span()));
            Ok(Readable::Stack)
        }
    }
}

fn apply_binary_shortcircuit<Op: BinaryOp>(
    bin: ast::Binary<'_>,
    polarity: bool,
    compiler: &mut Compiler,
) -> SourceResult<Readable> {
    // Compile the LHS.
    let lhs = bin.lhs().compile(compiler)?;
    if lhs == Readable::Empty {
        return bin.rhs().compile(compiler);
    }

    // Push the readable on the stack (forcing it).
    if lhs != Readable::Stack {
        compiler.push(Push::new(lhs, bin.lhs().span()));
    }

    // Duplicate the stack.
    compiler.push(Duplicate::new(bin.lhs().span()));

    // If the LHS is of the correct polarity, then execute the RHS, otherwise jump.
    let pointer = compiler.insert_pointer();

    // Pop the stack the remaining value if we did not jump.
    let jump_pointer = compiler.insert_pointer();

    let rhs_pointer = compiler.here();

    let rhs = bin.rhs().compile(compiler)?;
    compiler.push(Op::create(Readable::Stack, rhs, bin.span()));

    let end = compiler.here();

    compiler.write(
        pointer,
        JumpConditional::new(rhs_pointer, Readable::Stack, polarity, bin.span()),
    );

    compiler.write(jump_pointer, Jump::new(end, bin.span()));

    Ok(Readable::Stack)
}

fn apply_assignment<Op: AssignOp>(
    bin: ast::Binary<'_>,
    compiler: &mut Compiler,
) -> SourceResult<Readable> {
    let lhs = bin.lhs().access(compiler, false)?;
    let rhs = bin.rhs().compile(compiler)?;

    compiler.push(Op::create(lhs, rhs, bin.span()));
    Ok(Readable::Empty)
}

macro_rules! constant {
    ($name:ident => $transform:expr) => {
        impl Compile for ast::$name<'_> {
            fn compile(self, compiler: &mut Compiler<'_, '_>) -> SourceResult<Readable> {
                let fn_: fn(&mut Compiler<'_, '_>, ast::$name<'_>) -> Value = $transform;
                let value = fn_(compiler, self);
                Ok(add_constant(
                    compiler,
                    value,
                ))
            }
        }
    };
    ($($name:ident => $transform:expr),* $(,)*) => {
        $(constant!($name => $transform);)*
    }
}

fn add_constant(compiler: &mut Compiler, value: impl IntoValue) -> Readable {
    let value = value.into_value();
    compiler.constant(value)
}

constant! {
    Label => |_, label| {
        Label::new(PicoStr::intern(label.get())).into_value()
    },
    Text => |_, text| {
        TextElem::packed(text.get().clone()).spanned(text.span()).into_value()
    },
    Space => |_, _| {
        SpaceElem::shared().clone().into_value()
    },
    Linebreak => |_, _| {
        LinebreakElem::shared().clone().into_value()
    },
    Parbreak => |_, _| {
        ParbreakElem::shared().clone().into_value()
    },
    Escape => |_, escape| {
        Str::from(escape.get()).into_value()
    },
    Shorthand => |_, shorthand| {
        Value::Symbol(Symbol::single(shorthand.get()))
    },
    SmartQuote => |_, smart_quote| {
        SmartQuoteElem::new().with_double(smart_quote.double()).pack().into_value()
    },
}

impl Compile for ast::FieldAccess<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let field = self.field().get().clone();
        let target = self.target().compile(compiler)?;

        compiler.push(FieldAccess::new(
            target,
            self.target().span(),
            field,
            self.field().span(),
        ));

        Ok(Readable::Stack)
    }
}

impl Compile for ast::LetBinding<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        match self.kind() {
            ast::LetBindingKind::Normal(pattern) => {
                let value = self.init().map(|init| init.compile(compiler)).transpose()?;

                let pattern = pattern.pattern(compiler, true)?;

                if let Some(value) = value {
                    compiler.flow();
                    compiler.push(Destructure::new(value, pattern, self.span()));
                }
            }
            ast::LetBindingKind::Closure(ident) => {
                let slot = compiler.declare(ident.get(), self.span());
                let value = compile_let_closure(compiler, &self, &ident)?;

                compiler.push(Write::new(value, slot, self.span()));
            }
        }

        Ok(Readable::Empty)
    }
}

impl Compile for ast::DestructAssignment<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let value = self.value().compile(compiler)?;
        let pattern = self.pattern().pattern(compiler, false)?;

        compiler.flow();
        compiler.push(Destructure::new(value, pattern, self.span()));

        Ok(Readable::Empty)
    }
}

fn compile_let_closure(
    compiler: &mut Compiler,
    binding: &ast::LetBinding<'_>,
    closure_name: &ast::Ident<'_>,
) -> SourceResult<Readable> {
    let closure_span = closure_name.span();
    let closure_name = closure_name.get();

    // If there's no initializer, we can't create the closure.
    let Some(init) = binding.init() else {
        bail!(binding.span(), "closure declaration requires an initializer");
    };

    let ast::Expr::Closure(closure) = init else {
        bail!(init.span(), "expected closure expression");
    };

    // Compile the closure.
    compile_closure(
        compiler,
        closure_span,
        Some(closure.params()),
        closure.body(),
        Some(closure_name),
    )
}
