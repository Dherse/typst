use comemo::{Track, TrackedMut};
use ecow::{eco_format, eco_vec, EcoString, EcoVec};
use typst_library::diag::{
    bail, error, warning, At, FileError, SourceDiagnostic, SourceResult, Trace,
    Tracepoint,
};
use typst_library::engine::{Engine, Route, Sink};
use typst_library::foundations::{ops, Module, Value};
use typst_library::World;
use typst_syntax::ast::{self, AstNode, BareImportError};
use typst_syntax::package::{PackageManifest, PackageSpec};
use typst_syntax::{FileId, Span, VirtualPath};

use crate::compiler::locals::DeferredEntry;
use crate::eval;
use crate::vm::Readable;

use super::{Compile, Compiler, DeferredImports};

impl Compile for ast::ModuleImport<'_> {
    fn compile(self, compiler: &mut Compiler) -> SourceResult<Readable> {
        let source_expr = self.source();
        let source_span = source_expr.span();

        // Try and resolve the source.
        let Some(module) = source_expr.resolve(compiler) else {
            // if we cannot resolve the source, then we do it the dynamic way.
            return compile_dyn_module(self, compiler);
        };

        // Launch the loading of the module in another thread.
        let is_str = matches!(module, Value::Str(_));
        let deferred_index = load_deferred_module(
            module,
            source_span,
            compiler.engine,
            compiler.scope,
            &compiler.deferred,
        )?;

        // If there is a rename, import the source itself under that name.
        let new_name = self.new_name();
        if let Some(new_name) = new_name {
            if let ast::Expr::Ident(ident) = source_expr {
                if ident.as_str() == new_name.as_str() {
                    // Warn on `import x as x`
                    compiler.engine().sink.warn(warning!(
                        new_name.span(),
                        "unnecessary import rename to same name",
                    ));
                }
            }

            // Define renamed module on the scope.
            match &deferred_index {
                DeferredOutput::Immediate(value) => compiler.declare_constant(
                    new_name.get(),
                    new_name.span(),
                    value.clone(),
                ),
                DeferredOutput::Deferred(index) => compiler.declare_constant(
                    new_name.get(),
                    new_name.span(),
                    (*index, DeferredEntry::Module),
                ),
            };
        }

        match self.imports() {
            None => {
                if new_name.is_none() {
                    match self.bare_name() {
                        // Bare dynamic string imports are not allowed.
                        Ok(name)
                            if !is_str || matches!(source_expr, ast::Expr::Str(_)) =>
                        {
                            if matches!(source_expr, ast::Expr::Ident(_)) {
                                compiler.engine.sink.warn(warning!(
                                    source_expr.span(),
                                    "this import has no effect",
                                ));
                            }

                            match &deferred_index {
                                DeferredOutput::Immediate(value) => compiler
                                    .declare_constant(&name, source_span, value.clone()),
                                DeferredOutput::Deferred(index) => compiler
                                    .declare_constant(
                                        &name,
                                        source_span,
                                        (*index, DeferredEntry::Module),
                                    ),
                            };
                        }
                        Ok(_) | Err(BareImportError::Dynamic) => bail!(
                            source_span, "dynamic import requires an explicit name";
                            hint: "you can name the import with `as`"
                        ),
                        Err(BareImportError::PathInvalid) => bail!(
                            source_span, "module name would not be a valid identifier";
                            hint: "you can rename the import with `as`",
                        ),
                        // Bad package spec would have failed the import already.
                        Err(BareImportError::PackageInvalid) => unreachable!(),
                    }
                }
            }
            Some(ast::Imports::Wildcard) => {
                // this requires already waiting for the module...
                let module = match &deferred_index {
                    DeferredOutput::Immediate(value) => value.clone(),
                    DeferredOutput::Deferred(index) => compiler.deferred.wait(*index).0?,
                };

                // Then we just copy them as constants.
                // The other constants will still be resolved by the `Compiler::finish` fn
                // so special casing just makes things harder.
                for (var, binding) in module
                    .scope()
                    .ok_or_else(|| eco_vec![error!(self.span(), "expected a scope")])?
                    .iter()
                {
                    compiler.declare_constant(
                        var,
                        binding.span(),
                        binding.read().clone(),
                    );
                }
            }
            Some(ast::Imports::Items(items)) => {
                for item in items.iter() {
                    let path = item
                        .path()
                        .iter()
                        .map(|i| (i.span(), i.get().clone()))
                        .collect::<EcoVec<_>>();

                    match &deferred_index {
                        DeferredOutput::Immediate(value) => compiler.declare_constant(
                            item.bound_name().get(),
                            items.span(),
                            resolve_path(
                                value,
                                path.first().unwrap().0,
                                path.iter().map(|(span, name)| (*span, &**name)),
                            )
                            .map_err(|e| eco_vec![e])?
                            .clone(),
                        ),
                        DeferredOutput::Deferred(index) => compiler.declare_constant(
                            item.bound_name().get(),
                            items.span(),
                            (
                                *index,
                                DeferredEntry::Nested(path.first().unwrap().0, path),
                            ),
                        ),
                    };
                }
            }
        }

        Ok(Readable::Empty)
    }
}

pub enum DeferredOutput {
    Immediate(Value),
    Deferred(usize),
}

pub fn load_deferred_module<'scope>(
    source: Value,
    source_span: Span,
    engine: &mut Engine<'scope>,
    scope: &rayon::Scope<'scope>,
    imports: &DeferredImports<'scope>,
) -> SourceResult<DeferredOutput> {
    match source {
        Value::Func(func) => {
            if func.scope().is_none() {
                bail!(source_span, "cannot import from user-defined functions");
            }

            Ok(DeferredOutput::Immediate(Value::Func(func)))
        }
        value @ (Value::Type(_) | Value::Module(_)) => {
            Ok(DeferredOutput::Immediate(value))
        }
        Value::Str(path) => import_deferred(engine, scope, imports, &path, source_span)
            .map(DeferredOutput::Deferred),
        v => {
            bail!(
                source_span,
                "expected path, module, function, or type, found {}",
                v.ty()
            )
        }
    }
}

fn load_module(
    source: Value,
    source_span: Span,
    engine: &mut Engine,
) -> SourceResult<Value> {
    let mut source = source;
    match &source {
        Value::Func(func) => {
            if func.scope().is_none() {
                bail!(source_span, "cannot import from user-defined functions");
            }
        }
        Value::Type(_) => {}
        Value::Module(_) => {}
        Value::Str(path) => {
            source = Value::Module(import(engine, path, source_span)?);
        }
        v => {
            bail!(
                source_span,
                "expected path, module, function, or type, found {}",
                v.ty()
            )
        }
    }

    Ok(source)
}

fn compile_dyn_module(
    import: ast::ModuleImport<'_>,
    compiler: &mut Compiler,
) -> SourceResult<Readable> {
    if let Some(imports) = import.imports() {
        // module.import(compiler, self.span(), &imports)?;
        todo!()
    } else if import.new_name().is_none() {
        bail!(
            import.span(),
            "cannot import all items from a dynamic module";
            hint: "use `import \"...\" as x` to give a name to the module"
        )
    } else {
    }

    Ok(Readable::Empty)
}

pub trait Resolve {
    fn resolve(&self, compiler: &mut Compiler) -> Option<Value>;
}

impl Resolve for ast::Expr<'_> {
    fn resolve(&self, compiler: &mut Compiler) -> Option<Value> {
        // Allow: const var resolve, field accesses, static strings,
        // parenthesized, and basic string concat.
        match self {
            ast::Expr::Ident(ident) => ident.resolve(compiler),
            ast::Expr::Str(str) => Some(Value::Str(str.get().into())),
            ast::Expr::FieldAccess(field_access) => field_access.resolve(compiler),
            ast::Expr::Binary(bin) => bin.resolve(compiler),
            ast::Expr::Parenthesized(parenthesized) => {
                parenthesized.expr().resolve(compiler)
            }
            _ => None,
        }
    }
}

impl Resolve for ast::Ident<'_> {
    fn resolve(&self, compiler: &mut Compiler) -> Option<Value> {
        let variable = compiler.get(self.get(), self.span()).ok()?;

        compiler.resolve(variable)
    }
}

impl Resolve for ast::FieldAccess<'_> {
    fn resolve(&self, compiler: &mut Compiler) -> Option<Value> {
        let lhs = self.target().resolve(compiler)?;
        let sink = (compiler.engine(), self.field().span());
        lhs.field(&self.field(), sink).ok()
    }
}

impl Resolve for ast::Binary<'_> {
    fn resolve(&self, compiler: &mut Compiler) -> Option<Value> {
        match self.op() {
            ast::BinOp::Add => {
                let lhs = self.lhs().resolve(compiler)?;
                let rhs = self.rhs().resolve(compiler)?;

                ops::add(lhs, rhs).ok()
            }
            _ => None,
        }
    }
}

/// Process an import of a package or file relative to the current location.
pub fn import_deferred<'scope>(
    engine: &mut Engine<'scope>,
    scope: &rayon::Scope<'scope>,
    imports: &DeferredImports<'scope>,
    from: &str,
    span: Span,
) -> SourceResult<usize> {
    let routines = engine.routines;
    let world = engine.world;
    let introspector = engine.introspector;
    let traced = engine.traced;
    let route = engine.route.clone();
    if from.starts_with('@') {
        let spec = from.parse::<PackageSpec>().at(span)?;
        imports.insert_if_missing(spec.clone(), scope, move || {
            // We can create a new sink to fix lifetime values, we just need to collect
            // it along with the result!
            let mut sink = Sink::new();
            let mut engine = Engine {
                routines,
                world,
                introspector,
                traced,
                sink: sink.track_mut(),
                route: Route::extend(route.track()),
            };

            let mod_ = import_package(&mut engine, spec, span).map(Value::Module);
            (mod_, sink)
        })
    } else {
        let id = span.resolve_path(from).at(span)?;
        imports.insert_if_missing(id, scope, move || {
            // We can create a new sink to fix lifetime values, we just need to collect
            // it along with the result!
            let mut sink = Sink::new();
            let mut engine = Engine {
                routines,
                world,
                introspector,
                traced,
                sink: sink.track_mut(),
                route: Route::extend(route.track()),
            };

            let mod_ = import_file(&mut engine, id, span).map(Value::Module);
            (mod_, sink)
        })
    }
}

/// Process an import of a package or file relative to the current location.
pub fn import(engine: &mut Engine, from: &str, span: Span) -> SourceResult<Module> {
    if from.starts_with('@') {
        let spec = from.parse::<PackageSpec>().at(span)?;
        import_package(engine, spec, span)
    } else {
        let id = span.resolve_path(from).at(span)?;
        import_file(engine, id, span)
    }
}

/// Import a file from a path. The path is resolved relative to the given
/// `span`.
fn import_file(engine: &mut Engine, id: FileId, span: Span) -> SourceResult<Module> {
    // Load the source file.
    let source = engine.world.source(id).at(span)?;

    // Prevent cyclic importing.
    if engine.route.contains(source.id()) {
        bail!(span, "cyclic import");
    }

    // Evaluate the file.
    let point = || Tracepoint::Import;
    eval(
        engine.routines,
        engine.world,
        engine.traced,
        TrackedMut::reborrow_mut(&mut engine.sink),
        engine.route.track(),
        &source,
    )
    .trace(engine.world, point, span)
}

/// Import an external package.
fn import_package(
    engine: &mut Engine,
    spec: PackageSpec,
    span: Span,
) -> SourceResult<Module> {
    let (name, id) = resolve_package(engine, spec, span)?;
    import_file(engine, id, span).map(|module| module.with_name(name))
}

/// Resolve the name and entrypoint of a package.
fn resolve_package(
    engine: &mut Engine,
    spec: PackageSpec,
    span: Span,
) -> SourceResult<(EcoString, FileId)> {
    // Evaluate the manifest.
    let manifest_id = FileId::new(Some(spec.clone()), VirtualPath::new("typst.toml"));
    let bytes = engine.world.file(manifest_id).at(span)?;
    let string = bytes.as_str().map_err(FileError::from).at(span)?;
    let manifest: PackageManifest = toml::from_str(string)
        .map_err(|err| eco_format!("package manifest is malformed ({})", err.message()))
        .at(span)?;
    manifest.validate(&spec).at(span)?;

    // Evaluate the entry point.
    Ok((manifest.package.name, manifest_id.join(&manifest.package.entrypoint)))
}

pub(crate) fn resolve_path<'a>(
    value: &Value,
    span: Span,
    path: impl Iterator<Item = (Span, &'a str)>,
) -> Result<&Value, SourceDiagnostic> {
    let mut path = path.peekable();
    let Some(mut scope) = value.scope() else {
        return Err(error!(span, "unresolved import"));
    };

    while let Some((span, component)) = path.next() {
        let Some(binding) = scope.get(component) else {
            return Err(error!(span, "unresolved import"));
        };

        if path.peek().is_some() {
            // Nested import, as this is not the last component.
            // This must be a submodule.
            let value = binding.read();
            let Some(submodule) = value.scope() else {
                let error = if matches!(value, Value::Func(function) if function.scope().is_none())
                {
                    error!(span, "cannot import from user-defined functions")
                } else if !matches!(
                    value,
                    Value::Func(_) | Value::Module(_) | Value::Type(_)
                ) {
                    error!(
                        span,
                        "expected module, function, or type, found {}",
                        value.ty()
                    )
                } else {
                    panic!("unexpected nested import failure")
                };

                return Err(error);
            };

            // Walk into the submodule.
            scope = submodule;
        } else {
            return Ok(binding.read());
        }
    }

    Err(error!(span, "empty path"; hint: "this is a compiler bug"))
}
