use typst_syntax::ast::{self, AstNode};

use crate::diag::{bail, SourceResult};
use crate::engine::Engine;

use super::closure::compile_closure;
use super::{
    Compile, Compiler, PatternCompile, PatternItem, PatternKind, ReadableGuard,
    WritableGuard,
};

impl Compile for ast::LetBinding<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        _: WritableGuard,
    ) -> SourceResult<()> {
        self.compile_to_readable(compiler, engine)?;

        Ok(())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<ReadableGuard> {
        match self.kind() {
            ast::LetBindingKind::Normal(pattern) => {
                compile_normal(compiler, engine, self, &pattern)?;
            }
            ast::LetBindingKind::Closure(closure) => {
                compile_let_closure(compiler, engine, self, &closure)?;
            }
        }

        Ok(ReadableGuard::None)
    }
}

impl Compile for ast::DestructAssignment<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        _: WritableGuard,
    ) -> SourceResult<()> {
        self.compile_to_readable(compiler, engine)?;

        Ok(())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<ReadableGuard> {
        // We compile the pattern and add it to the local scope.
        let pattern = self
            .pattern()
            .compile_pattern(compiler, engine, false)?
            .with_declare(true);

        // We destructure the initializer using the pattern.
        // Simple patterns can be directly stored.
        if let PatternKind::Single(PatternItem::Simple(span, access, _)) = &pattern.kind {
            let Some(guard) = compiler.get_access(access).unwrap().as_simple() else {
                bail!(*span, "cannot destructure into a non-writable access");
            };

            self.value().compile(compiler, engine, guard.clone().into())?;
        } else {
            let value = self.value().compile_to_readable(compiler, engine)?;
            let pattern_id = compiler.pattern(pattern);

            compiler.destructure(self.span(), value, pattern_id);
        }

        Ok(ReadableGuard::None)
    }
}

fn compile_normal(
    compiler: &mut Compiler,
    engine: &mut Engine,
    binding: &ast::LetBinding<'_>,
    pattern: &ast::Pattern<'_>,
) -> SourceResult<()> {
    // Simple patterns can be directly stored.
    if let ast::Pattern::Normal(ast::Expr::Ident(ident)) = pattern {
        let guard = compiler.allocate();
        if let Some(init) = binding.init() {
            init.compile(compiler, engine, guard.clone().into())?;
        }
        compiler.declare_to_register(ident.span(), ident.get().as_str(), guard);
    } else {
        // We destructure the initializer using the pattern.
        let value = if let Some(init) = binding.init() {
            init.compile_to_readable(compiler, engine)?
        } else {
            ReadableGuard::None
        };

        // We compile the pattern.
        let pattern = pattern.compile_pattern(compiler, engine, true)?.with_declare(true);
        let pattern_id = compiler.pattern(pattern);

        // We destructure the initializer using the pattern.
        compiler.flow();
        compiler.destructure(binding.span(), value, pattern_id);
    }

    Ok(())
}

fn compile_let_closure(
    compiler: &mut Compiler,
    engine: &mut Engine,
    binding: &ast::LetBinding<'_>,
    closure_name: &ast::Ident<'_>,
) -> SourceResult<()> {
    let closure_span = closure_name.span();
    let closure_name = closure_name.get();

    // If there's no initializer, we can't create the closure.
    let Some(init) = binding.init() else {
        bail!(binding.span(), "closure declaration requires an initializer");
    };

    let ast::Expr::Closure(closure) = init else {
        bail!(init.span(), "expected closure expression");
    };

    // We create the local.
    let local = compiler.declare(closure_span, closure_name);

    // Compile the closure.
    compile_closure(
        compiler,
        engine,
        closure_span,
        closure.params(),
        closure.body(),
        local.into(),
        Some(closure_name),
    )
}
