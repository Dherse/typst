use std::sync::Arc;

use ecow::{eco_format, EcoString};
use typst_library::diag::{bail, SourceResult};
use typst_library::foundations::{Func, IntoValue};
use typst_syntax::ast::{self, AstNode};
use typst_syntax::Span;
use typst_utils::LazyHash;

use crate::closure::CompiledClosure;
use crate::compiler::pattern::PatternCompile;
use crate::compiler::{CompileTopLevel, InstructionList};
use crate::vm::instructions::{Instantiate, Destructure, Contextual, Join};
use crate::vm::{Readable, Pattern};
use crate::CompiledParam;

use super::{Compile, Compiler};

impl Compile for ast::Closure<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        compile_closure(compiler, self.span(), Some(self.params()), self.body(), None)
    }
}

impl Compile for ast::Contextual<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let closure = compile_closure(compiler, self.span(), None, self.body(), None)?;

        compiler.push(Contextual::new(closure, self.span()));

        Ok(Readable::Stack)
    }
}

pub fn compile_closure(
    compiler: &mut Compiler,
    closure_span: Span,
    params: Option<ast::Params>,
    body: ast::Expr,
    name: Option<&EcoString>,
) -> SourceResult<Readable> {
    // Evaluate default values of named parameters.
    let mut defaults = Vec::new();
    for param in params.iter().flat_map(|p| p.children()) {
        if let ast::Param::Named(named) = param {
            let reg = named.expr().compile(compiler)?;
            defaults.push(reg);
        }
    }

    // Create a new compiler for the closure.
    let mut instructions = InstructionList::default();
    let mut closure_compiler = Compiler::new_closure(
        compiler.engine,
        &compiler.locals,
        &compiler.deferred,
        compiler.scope,
        &mut instructions,
    );

    // Create the local such that the closure can use itself.
    let closure_local = name.map(|name| closure_compiler.declare(name, closure_span));

    // Build the parameter list of the closure.
    let mut parameters =
        Vec::with_capacity(params.iter().flat_map(|p| p.children()).count());
    let mut defaults_iter = defaults.iter();
    for (i, param) in params.iter().flat_map(|p| p.children()).enumerate() {
        match param {
            ast::Param::Pos(pat) => {
                // Compile the pattern.
                let pattern = pat.pattern(&mut closure_compiler, true)?;

                if let Pattern::Single(item) = pattern {
                    let Some(name) = closure_compiler.get_name(item.head) else {
                        bail!(pat.span(), "missing name for slot"; hint: "this is a compiler bug");
                    };

                    parameters.push(CompiledParam::Pos(item.head, name));

                    continue;
                }

                // Create the local variable.
                let name = eco_format!("arg{i}");
                let target = closure_compiler.declare(&name, param.span());

                parameters.push(CompiledParam::Pos(
                    target,
                    EcoString::from("pattern parameter"),
                ));

                // Destructure the argument
                closure_compiler.push(Destructure::new(
                    Readable::Slot(target),
                    pattern,
                    param.span(),
                ));
            }
            ast::Param::Named(named) => {
                // Create the local variable.
                let name = named.name().get();
                let target = closure_compiler.declare(name, named.name().span());

                // Add the parameter to the list.
                parameters.push(CompiledParam::Named {
                    span: named.span(),
                    target: target.into(),
                    name: name.clone(),
                    default: defaults_iter.next().map(|r| r.clone().into()),
                });
            }
            ast::Param::Spread(spread) => {
                let Some(name) = spread.sink_ident() else {
                    // Add the parameter to the list.
                    parameters.push(CompiledParam::Sink(
                        spread.span(),
                        None,
                        EcoString::from(".."),
                    ));
                    continue;
                };

                // Create the local variable.
                let target = closure_compiler.declare(name.get(), name.span());

                parameters.push(CompiledParam::Sink(
                    spread.span(),
                    Some(target),
                    EcoString::from(".."),
                ));
            }
        }
    }

    // Compile the body of the closure.
    let mut display = false;
    match body {
        ast::Expr::CodeBlock(code) => {
            code.body().compile_top_level(&mut closure_compiler)?;
        }
        ast::Expr::ContentBlock(content) => {
            content.body().compile_top_level(&mut closure_compiler)?;
            display = true;
        }
        other => {
            let out = other.compile(&mut closure_compiler)?;
            if !matches!(out, Readable::Empty | Readable::None) {
                closure_compiler.push(Join::new(out, other.span()));
            }

            display = other.is_display();
        }
    }

    // Ensure that a flow event is present.
    closure_compiler.flow();

    // Collect the compiled closure.
    let compiled_closure = Arc::new(LazyHash::new(closure_compiler.finish_closure(
        params.map_or(closure_span, |p| p.span()),
        name.cloned(),
        parameters,
        closure_local,
        display,
    )?));

    // Get the closure ID.
    let compiled = CompiledClosure::new(compiled_closure, &*compiler)?;

    // Instantiate the closure.
    match compiled {
        CompiledClosure::Instanciated(closure) => {
            Ok(compiler.constant(Func::from(closure).into_value()))
        }
        CompiledClosure::Closure(closure) => {
            compiler.push(Instantiate::new(closure, closure_span));
            Ok(Readable::Stack)
        }
    }
}
