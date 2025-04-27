use typst_library::diag::SourceResult;
use typst_syntax::ast::{self, AstNode};

use crate::compiler::locals::DeferredEntry;
use crate::compiler::{load_deferred_module, Resolve};
use crate::vm::Readable;

use super::import::DeferredOutput;
use super::{Compile, Compiler};

impl Compile for ast::ModuleInclude<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let source_expr = self.source();
        let source_span = source_expr.span();

        // Try and resolve the source.
        let Some(module) = source_expr.resolve(compiler) else {
            // if we cannot resolve the source, then we do it the dynamic way.
            todo!()
        };

        // Launch the loading of the module in another thread.
        let deferred = load_deferred_module(
            module,
            source_span,
            compiler.engine,
            compiler.scope,
            &compiler.deferred,
        )?;

        Ok(match deferred {
            DeferredOutput::Immediate(value) => compiler.constant(value),
            DeferredOutput::Deferred(index) => {
                compiler.constant((index, DeferredEntry::Content(self.span())))
            }
        })
    }
}
