use ecow::EcoString;
use typst_syntax::ast::{self, AstNode};
use typst_syntax::Span;
use typst_utils::PicoStr;

use crate::diag::{bail, SourceResult};
use crate::engine::Engine;
use crate::foundations::Func;
use crate::lang::compiled::{CompiledClosure, CompiledParam};
use crate::lang::compiler::{
    Access, CompileTopLevel, PatternCompile, PatternItem, PatternKind,
};

use super::{Compile, Compiler, ReadableGuard, WritableGuard};

impl Compile for ast::Closure<'_> {
    fn compile(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
        output: WritableGuard,
    ) -> SourceResult<()> {
        compile_closure(
            compiler,
            engine,
            self.span(),
            self.params(),
            self.body(),
            Some(output),
            None,
        )
        .map(|_| ())
    }

    fn compile_to_readable(
        &self,
        compiler: &mut Compiler<'_>,
        engine: &mut Engine,
    ) -> SourceResult<ReadableGuard> {
        compile_closure(
            compiler,
            engine,
            self.span(),
            self.params(),
            self.body(),
            None,
            None,
        )
    }
}

pub fn compile_closure(
    compiler: &mut Compiler,
    engine: &mut Engine,
    closure_span: Span,
    params: ast::Params,
    body: ast::Expr,
    output: Option<WritableGuard>,
    name: Option<&EcoString>,
) -> SourceResult<ReadableGuard> {
    // Evaluate default values of named parameters.
    let mut defaults = Vec::new();
    for param in params.children() {
        if let ast::Param::Named(named) = param {
            let reg = named.expr().compile_to_readable(compiler, engine)?;
            defaults.push(reg);
        }
    }

    // Create a new compiler for the closure.
    let mut closure_compiler = Compiler::new_closure(compiler, name.cloned());

    // Create the local such that the closure can use itself.
    let closure_local =
        name.map(|name| closure_compiler.declare(closure_span, name.as_str()));

    // Build the parameter list of the closure.
    let mut parameters = Vec::with_capacity(params.children().count());
    let mut defaults_iter = defaults.iter();
    for param in params.children() {
        match param {
            ast::Param::Pos(pat) => {
                // Compile the pattern.
                let pattern = pat.compile_pattern(&mut closure_compiler, engine, true)?;

                if let PatternKind::Single(PatternItem::Simple(_, access, name)) =
                    &pattern.kind
                {
                    let Some(reg) =
                        closure_compiler.get_access(access).and_then(Access::as_simple)
                    else {
                        bail!(
                            pat.span(),
                            "expected a writable location for param";
                            hint: "this is a compiler bug"
                        );
                    };

                    parameters.push(CompiledParam::Pos(reg.clone().into(), *name));

                    continue;
                }

                let reg = closure_compiler.allocate();
                let pattern_id = closure_compiler.pattern(pattern);

                parameters.push(CompiledParam::Pos(
                    reg.clone().into(),
                    PicoStr::from("pattern parameter"),
                ));

                closure_compiler.destructure(pat.span(), reg, pattern_id);
            }
            ast::Param::Named(named) => {
                // Create the local variable.
                let name = named.name().get().as_str();
                let target = closure_compiler.declare(named.name().span(), name);

                // Add the parameter to the list.
                parameters.push(CompiledParam::Named {
                    span: named.span(),
                    target: target.into(),
                    name: PicoStr::new(name),
                    default: defaults_iter.next().map(|r| r.clone().into()),
                });
            }
            ast::Param::Spread(spread) => {
                let Some(name) = spread.sink_ident() else {
                    // Add the parameter to the list.
                    parameters.push(CompiledParam::Sink(
                        spread.span(),
                        None,
                        PicoStr::from(".."),
                    ));
                    continue;
                };

                // Create the local variable.
                let target = closure_compiler.declare(name.span(), name.as_str());

                parameters.push(CompiledParam::Sink(
                    spread.span(),
                    Some(target.as_register()),
                    PicoStr::from(".."),
                ));
            }
        }
    }

    // Compile the body of the closure.
    let mut display = false;
    match body {
        ast::Expr::Code(code) => {
            code.body().compile_top_level(&mut closure_compiler, engine)?;
        }
        ast::Expr::Content(content) => {
            content.body().compile_top_level(&mut closure_compiler, engine)?;
            display = true;
        }
        other => {
            other.compile(&mut closure_compiler, engine, WritableGuard::Joined)?;
            display = other.is_display();
        }
    }

    // Ensure that a flow event is present.
    closure_compiler.flow();

    // Collect the compiled closure.
    let compiled_closure = closure_compiler.finish_closure(
        params.span(),
        parameters,
        closure_local,
        display,
    )?;

    // Get the closure ID.
    let compiled = CompiledClosure::new(compiled_closure, &*compiler);

    // Instantiate the closure.
    if let CompiledClosure::Instanciated(closure) = compiled {
        let const_id = compiler.const_(Func::from(closure));
        if let Some(output) = output {
            compiler.copy(closure_span, const_id, output.clone());

            // We can ignore the error since if we're writing to a `Joiner`,
            // then the value isn't used either way.
            Ok(output.try_into().unwrap_or(ReadableGuard::None))
        } else {
            Ok(const_id.into())
        }
    } else {
        let closure_id = compiler.closure(compiled);
        if let Some(output) = output {
            compiler.instantiate(closure_span, closure_id, output.clone());

            // We can ignore the error since if we're writing to a `Joiner`,
            // then the value isn't used either way.
            Ok(output.try_into().unwrap_or(ReadableGuard::None))
        } else {
            let reg = compiler.allocate();
            compiler.instantiate(closure_span, closure_id, reg.clone());
            Ok(reg.into())
        }
    }
}
