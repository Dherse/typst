use std::borrow::Cow;
use std::ops::Deref;

use comemo::Tracked;
use ecow::eco_format;
use smallvec::SmallVec;
use typst_syntax::{Span, Spanned};

use crate::diag::{
    bail, error, At, Hint, HintedStrResult, HintedString, SourceResult, Trace, Tracepoint,
};
use crate::engine::Engine;
use crate::foundations::{
    call_method_access, call_method_mut, missing_method, Arg, Args,
    Bytes, Content, Context, Func, IntoValue, NativeElement, Str, Value,
};
use crate::lang::compiled::{CompiledAccess, CompiledAccessRoot, CompiledAccessSegment};
use crate::lang::interpreter::methods::ValueAccessor;
use crate::lang::operands::Readable;
use crate::math::{Accent, AccentElem, LrElem};
use crate::symbols::Symbol;
use crate::text::TextElem;

use super::Vm;

pub enum AccessedValue<'a> {
    Borrowed(&'a Value),
    Owned(Value),
    Mut(&'a mut Value),
}

impl AccessedValue<'_> {
    pub fn borrowed<'d>(&'d mut self, mutable: bool) -> AccessedValue<'d> {
        match self {
            Self::Borrowed(value) => AccessedValue::Borrowed(&**value),
            Self::Owned(value) => AccessedValue::Borrowed(&*value),
            Self::Mut(value) => {
                if mutable {
                    AccessedValue::Mut(&mut **value)
                } else {
                    AccessedValue::Borrowed(&**value)
                }
            }
        }
    }

    pub fn into_owned(self) -> AccessedValue<'static> {
        match self {
            Self::Borrowed(value) => AccessedValue::Owned(value.clone()),
            Self::Owned(value) => AccessedValue::Owned(value),
            Self::Mut(value) => AccessedValue::Owned(value.clone()),
        }
    }

    pub fn into_value(self) -> Value {
        match self {
            Self::Borrowed(value) => value.clone(),
            Self::Owned(value) => value,
            Self::Mut(value) => value.clone(),
        }
    }

    pub fn write(self, value: Value) -> HintedStrResult<()> {
        self.write_transformed(|_| Ok(value))
    }

    pub fn write_transformed(
        self,
        f: impl FnOnce(Value) -> HintedStrResult<Value>,
    ) -> HintedStrResult<()> {
        match self {
            Self::Borrowed(_) => read_only(),
            Self::Owned(_) => temporary(),
            Self::Mut(mut_) => {
                let old = std::mem::take(mut_);
                *mut_ = f(old)?;
                Ok(())
            }
        }
    }
}

impl<'a> Deref for AccessedValue<'a> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Borrowed(value) => value,
            Self::Owned(value) => value,
            Self::Mut(value) => value,
        }
    }
}

impl CompiledAccess {
    pub fn get_value<'a: 'b, 'b, 'c>(
        &'a self,
        vm: &'b mut Vm<'a, 'c>,
        engine: &mut Engine,
    ) -> SourceResult<Value> {
        self.get(vm, engine, false, |value| Ok(value.into_value()))
    }

    pub fn get<'a: 'b, 'b, 'c, O>(
        &'a self,
        vm: &'b mut Vm<'a, 'c>,
        engine: &mut Engine,
        declare: bool,
        accessor: impl for<'d> FnOnce(AccessedValue<'d>) -> SourceResult<O>,
    ) -> SourceResult<O> {
        // Get the context.
        let context = vm.context;

        // Get all of the arguments at once.
        // This is a `DoubleEndedIterator` so reversing it is cheap.
        let mut args = self
            .segments
            .iter()
            .rev()
            .filter_map(|segment| match segment {
                CompiledAccessSegment::Method { args, .. } => match args {
                    Readable::Reg(reg) => Some(vm.take(*reg).into_owned()),
                    other => Some(vm.read(*other).clone()),
                },
                _ => None,
            })
            .collect::<SmallVec<[_; 4]>>();

        // Get the root path.
        let mut root = self.root.get(engine, vm, self.in_math)?;

        // Force borrowing to allow chaining of accesses.
        let value = root.borrowed(self.mutable);

        fn access_inner<O>(
            declare: bool,
            mut parent: AccessedValue,
            segments: &[CompiledAccessSegment],
            args: &mut SmallVec<[Value; 4]>,
            engine: &mut Engine,
            context: Tracked<Context>,
            accessor: impl for<'d> FnOnce(AccessedValue<'d>) -> SourceResult<O>,
            in_math: bool,
        ) -> SourceResult<O> {
            if segments.is_empty() {
                return accessor(parent);
            }

            let is_last = segments.len() == 1;
            let this = match &segments[0] {
                CompiledAccessSegment::Field { span, name } => match parent {
                    AccessedValue::Borrowed(ref_) => {
                        field_ref(ref_, *name, context).at(*span)?
                    }
                    AccessedValue::Owned(owned) => {
                        field_ref(&owned, *name, context).at(*span)?.into_owned()
                    }
                    AccessedValue::Mut(mut_) => {
                        field_mut(mut_, *name, declare).at(*span)?
                    }
                },
                CompiledAccessSegment::Method {
                    span,
                    name_span,
                    name,
                    trailing_comma,
                    ..
                } => {
                    let mut args = match args.pop().unwrap() {
                        Value::None => Args::new::<Value>(*span, []),
                        Value::Args(args) => args,
                        other => {
                            bail!(
                                *span,
                                "expected arguments or none, found {}",
                                other.ty().long_name()
                            );
                        }
                    };

                    let requires_mut = (*parent).is_mut(*name);
                    let requires_access_mut = (*parent).is_access_mut(*name);

                    // If we're:
                    // - in a math context,
                    // - at the last segment,
                    // - not requiring mutability,
                    // - not requiring access mutability,
                    // then we can try and check whether we are in a math call.
                    if in_math && is_last && !requires_mut && !requires_access_mut {
                        // Re-borrow the parent immutably.
                        let parent = parent.borrowed(false).into_value();

                        // Get the callee which is a field, if it's not a field then
                        // we are not in a math call.
                        let callee =
                            get_callee(parent, *span, name, *name_span, &mut args)?;
                        if !matches!(callee, Value::Func(_)) {
                            let accessed =
                                math_call(*span, callee, &mut args, *trailing_comma)
                                    .map(AccessedValue::Owned)?;

                            return accessor(accessed);
                        }
                    }

                    match parent {
                        AccessedValue::Mut(mut_) if requires_mut => {
                            let point = || Tracepoint::Call(Some((*name).into()));
                            let value =
                                call_method_mut(mut_, *name, args, *span, *name_span)
                                    .trace(engine.world, point, *span)?
                                    .spanned(*span);

                            AccessedValue::Owned(value)
                        }
                        AccessedValue::Mut(mut_) if requires_access_mut => {
                            let point = || Tracepoint::Call(Some((*name).into()));
                            let value =
                                call_method_access(mut_, *name, args, *span, *name_span)
                                    .trace(engine.world, point, *span)?;

                            AccessedValue::Mut(value)
                        }
                        AccessedValue::Borrowed(_) if requires_mut => {
                            return read_only().at(*span);
                        }
                        AccessedValue::Owned(_) if requires_mut => {
                            return temporary().at(*span);
                        }
                        AccessedValue::Mut(mut_) => {
                            let point = || Tracepoint::Call(Some((*name).into()));
                            let value = call_method_immutable(
                                engine, context, mut_, *name, args, *span, *name_span,
                            )
                            .trace(engine.world, point, *span)?
                            .spanned(*span);

                            AccessedValue::Owned(value)
                        }
                        AccessedValue::Borrowed(ref_) => {
                            let point = || Tracepoint::Call(Some((*name).into()));
                            let value = call_method_immutable(
                                engine, context, ref_, *name, args, *span, *name_span,
                            )
                            .trace(engine.world, point, *span)?
                            .spanned(*span);

                            AccessedValue::Owned(value)
                        }
                        AccessedValue::Owned(owned) => {
                            let point = || Tracepoint::Call(Some((*name).into()));
                            let value = call_method_immutable(
                                engine, context, &owned, *name, args, *span, *name_span,
                            )
                            .trace(engine.world, point, *span)?
                            .spanned(*span);

                            AccessedValue::Owned(value)
                        }
                    }
                }
            };

            access_inner(
                declare,
                this,
                &segments[1..],
                args,
                engine,
                context,
                accessor,
                in_math,
            )
        }

        // Use recursive call for recursive mutable borrow.
        access_inner(
            declare,
            value,
            &self.segments,
            &mut args,
            engine,
            context,
            accessor,
            self.in_math,
        )
    }
}

fn call_method_immutable(
    engine: &mut Engine,
    context: Tracked<Context>,
    value: &Value,
    method: &str,
    mut args: Args,
    span: Span,
    name_span: Span,
) -> SourceResult<Value> {
    let ty = value.ty();
    let missing = || missing_method(ty, method).at(name_span);

    // Special case for plugins.
    if let Value::Plugin(plugin) = value {
        let bytes = args.all::<Bytes>()?;
        args.finish()?;

        let out = plugin.call(method, bytes).at(span)?.into_value();

        return Ok(out);
    }

    // Special case for dictionaries.
    if let Value::Dict(dict) = value {
        if method != "at" && dict.contains(method) {
            return missing_method(ty, method)
                .hint(eco_format!(
                    "to call the function stored in the dictionary, surround \
                     the field access with parentheses, e.g. `(dict.{method})(..)`",
                ))
                .at(span);
        }
    }

    // Array methods.
    if let Value::Array(array) = value {
        match method {
            "first" => return array.first().at(span),
            "last" => return array.last().at(span),
            "at" => {
                return array.at(args.expect("index")?, args.named("default")?).at(span)
            }
            _ => (),
        }
    }

    // Dict methods.
    if let Value::Dict(dict) = value {
        match method {
            "at" => return dict.at(args.expect("key")?, args.named("default")?).at(span),
            _ => (),
        }
    }

    // Associated methods.
    if let Value::Type(ty_) = value {
        if let Some(func) = ty_.scope().get(method) {
            let point = || Tracepoint::Call(Some(method.into()));
            return Ok(func
                .call(engine, context, span, args)
                .trace(engine.world, point, span)?
                .spanned(span));
        }
    }

    // Functions in module.
    if let Value::Module(module) = value {
        let func = module.field(method).at(span)?;
        let point = || Tracepoint::Call(Some(method.into()));
        return Ok(func
            .call(engine, context, span, args)
            .trace(engine.world, point, span)?
            .spanned(span));
    }

    // Functions in functions.
    if let Value::Func(func) = value {
        if let Some(func) = func.scope().and_then(|scope| scope.get(method)) {
            let point = || Tracepoint::Call(Some(method.into()));
            return Ok(func
                .call(engine, context, span, args)
                .trace(engine.world, point, span)?
                .spanned(span));
        }
    }

    // Special case for methods.
    let callee = if let Some(callee) = value.ty().scope().get(method) {
        let this = Arg {
            span,
            name: None,
            value: Spanned::new(value.clone(), span),
        };
        args.items.insert(0, this);

        Cow::Borrowed(callee)
    } else {
        return missing();
    };

    // Call the function.
    let point = || Tracepoint::Call(callee.name().map(Into::into));
    let value = callee
        .call(engine, context, span, args)
        .trace(engine.world, point, span)?
        .spanned(span);

    Ok(value)
}

fn field_ref<'a>(
    value: &'a Value,
    field: &'static str,
    context: Tracked<Context>,
) -> HintedStrResult<AccessedValue<'a>> {
    let err = match value.field(field) {
        Ok(value) => return Ok(AccessedValue::Owned(value)),
        Err(err) => err,
    };

    // Check whether this is a get rule field access.
    if_chain::if_chain! {
        if let Value::Func(func) = &*value;
        if let Some(element) = func.element();
        if let Some(id) = element.field_id(field);
        let styles = context.styles();
        if let Some(value) = element.field_from_styles(
            id,
            styles.as_ref().map(|&s| s).unwrap_or_default(),
        );
        then {
            // Only validate the context once we know that this is indeed
            // a field from the style chain.
            let _ = styles?;
            return Ok(AccessedValue::Owned(value));
        }
    }

    Err(HintedString::new(err))
}

fn field_mut<'a>(
    value: &'a mut Value,
    field: &'static str,
    declare: bool,
) -> HintedStrResult<AccessedValue<'a>> {
    match value {
        Value::Dict(dict) => {
            if declare {
                Ok(AccessedValue::Mut(dict.at_mut_or_insert(Str::from(field))))
            } else {
                dict.at_mut(field).map(AccessedValue::Mut)
            }
        }
        value => {
            let ty = value.ty();
            if matches!(
                value, // those types have their own field getters
                Value::Symbol(_) | Value::Content(_) | Value::Module(_) | Value::Func(_)
            ) {
                bail!("cannot mutate fields on {ty}");
            } else if crate::foundations::fields_on(ty).is_empty() {
                bail!("{ty} does not have accessible fields");
            } else {
                // type supports static fields, which don't yet have
                // setters
                bail!(
                    "fields on {ty} are not yet mutable";
                    hint: "try creating a new {ty} with the updated field value instead"
                )
            }
        }
    }
}

impl CompiledAccessRoot {
    pub fn get<'a: 'b, 'b, 'c>(
        &'a self,
        engine: &mut Engine,
        vm: &'b mut Vm<'a, 'c>,
        in_math: bool,
    ) -> SourceResult<AccessedValue<'b>> {
        Ok(match self {
            Self::Register(reg) => {
                let value = vm.write(*reg).unwrap();
                AccessedValue::Mut(value)
            }
            Self::Readable(readable) => {
                let value = vm.read(*readable);
                AccessedValue::Borrowed(value)
            }
            Self::Call { span, func, args, trailing_comma } => {
                let func = vm.read(*func);
                let args = vm.read(*args);
                let mut args = match args {
                    Value::Args(args) => args.clone(),
                    Value::None => Args::with_capacity(*span, 0),
                    _ => bail!(
                        *span,
                        "expected argumentss, found {}",
                        args.ty().long_name()
                    ),
                };

                if in_math && !matches!(func, Value::Func(_)) {
                    return math_call(*span, func.clone(), &mut args, *trailing_comma)
                        .map(AccessedValue::Owned);
                }

                let func = func.clone().cast::<Func>().at(*span)?;

                let value = func.call(engine, vm.context, args)?;

                AccessedValue::Owned(value.spanned(*span))
            }
            Self::Value(value) => AccessedValue::Borrowed(value),
        })
    }
}

fn get_callee(
    target: Value,
    target_span: Span,
    field: &str,
    field_span: Span,
    args: &mut Args,
) -> SourceResult<Value> {
    // Prioritize associated functions on the value's type (i.e.,
    // methods) over its fields. A function call on a field is only
    // allowed for functions, types, modules (because they are scopes),
    // and symbols (because they have modifiers).
    //
    // For dictionaries, it is not allowed because it would be ambiguous
    // (prioritizing associated functions would make an addition of a
    // new associated function a breaking change and prioritizing fields
    // would break associated functions for certain dictionaries).
    if let Some(callee) = target.ty().scope().get(&field) {
        let this = Arg {
            span: target_span,
            name: None,
            value: Spanned::new(target, target_span),
        };
        args.items.insert(0, this);
        Ok(callee.clone())
    } else if matches!(
        target,
        Value::Symbol(_) | Value::Func(_) | Value::Type(_) | Value::Module(_)
    ) {
        Ok(target.field(&field).at(field_span)?)
    } else {
        let mut error =
            error!(field_span, "type {} has no method `{field}`", target.ty(),);

        let mut field_hint = || {
            if target.field(&field).is_ok() {
                error.hint(eco_format!("did you mean to access the field `{field}`?",));
            }
        };

        match target {
            Value::Dict(ref dict) => {
                if matches!(dict.get(&field), Ok(Value::Func(_))) {
                    error.hint(eco_format!(
                        "to call the function stored in the dictionary, surround \
                         the field access with parentheses, e.g. `(dict.{field})(..)`"
                    ));
                } else {
                    field_hint();
                }
            }
            _ => field_hint(),
        }

        bail!(error);
    }
}

fn math_call(
    callee_span: Span,
    callee: Value,
    args: &mut Args,
    trailing_comma: bool,
) -> SourceResult<Value> {
    if let Value::Symbol(sym) = &callee {
        let c = sym.get();
        if let Some(accent) = Symbol::combining_accent(c) {
            let base = args.expect("base")?;
            let size = args.named("size")?;
            std::mem::take(args).finish()?;
            let mut accent = AccentElem::new(base, Accent::new(accent));
            if let Some(size) = size {
                accent = accent.with_size(size);
            }
            return Ok(Value::Content(accent.pack()));
        }
    }

    let mut body = Content::empty();
    for (i, arg) in args.all::<Content>()?.into_iter().enumerate() {
        if i > 0 {
            body += TextElem::packed(',');
        }
        body += arg;
    }
    if trailing_comma {
        body += TextElem::packed(',');
    }

    Ok(Value::Content(
        callee.display().spanned(callee_span)
            + LrElem::new(TextElem::packed('(') + body + TextElem::packed(')')).pack(),
    ))
}

#[cold]
fn read_only<T>() -> HintedStrResult<T> {
    bail!(
        "cannot mutate a read-only value";
        hint: "try storing the value in a variable first"
    )
}

#[cold]
fn temporary<T>() -> HintedStrResult<T> {
    bail!(
        "cannot mutate a temporary value";
        hint: "try storing the value in a variable first"
    )
}

/*impl CompiledAccess {
    /// Gets the value using read-only access.
    pub fn read<'a: 'b, 'b>(
        &'a self,
        span: Span,
        vm: &'b Vm<'a, '_>,
    ) -> SourceResult<Cow<'b, Value>> {
        match self {
            CompiledAccess::Register(reg) => Ok(Cow::Borrowed(reg.read(vm))),
            CompiledAccess::Module(module) => Ok(Cow::Borrowed(module)),
            CompiledAccess::Func(func) => Ok(Cow::Borrowed(func)),
            CompiledAccess::Value(value) => Ok(Cow::Borrowed(value)),
            CompiledAccess::Type(ty) => Ok(Cow::Borrowed(ty)),
            CompiledAccess::Chained(_, value, field, field_span) => {
                let access = vm.read(*value);
                let value = access.read(span, vm)?;
                if let Some(assoc) = value.ty().scope().get(field) {
                    let Value::Func(method) = assoc else {
                        bail!(
                            span,
                            "expected function, found {}",
                            assoc.ty().long_name()
                        );
                    };

                    let mut args = Args::new(span, std::iter::once(value.into_owned()));

                    Ok(Cow::Owned(
                        method.clone().with(&mut args).into_value().spanned(span),
                    ))
                } else {
                    let err = match value.field(field).at(*field_span) {
                        Ok(value) => return Ok(Cow::Owned(value)),
                        Err(err) => err,
                    };

                    // Check whether this is a get rule field access.
                    if_chain::if_chain! {
                        if let Value::Func(func) = &*value;
                        if let Some(element) = func.element();
                        if let Some(id) = element.field_id(field);
                        let styles = vm.context.styles().at(*field_span);
                        if let Some(value) = element.field_from_styles(
                            id,
                            styles.as_ref().map(|&s| s).unwrap_or_default(),
                        );
                        then {
                            // Only validate the context once we know that this is indeed
                            // a field from the style chain.
                            let _ = styles?;
                            return Ok(Cow::Owned(value));
                        }
                    }

                    Err(err)
                }
            }
            CompiledAccess::AccessorMethod(value, method, args) => {
                // Get the callee.
                let access = vm.read(*value);
                let value = access.read(span, vm)?;

                // Get the arguments.
                let args = vm.read(*args);
                let mut args = match args {
                    Value::Args(args) => args.clone(),
                    Value::None => Args::with_capacity(span, 0),
                    _ => bail!(
                        span,
                        "expected argumentss, found {}",
                        args.ty().long_name()
                    ),
                };

                // Call the method.
                let ty = value.ty();
                let missing = || Err(missing_method(ty, method)).at(span);

                let accessed = match &*value {
                    Value::Array(array) => {
                        if *method == "first" {
                            array.first().at(span)?
                        } else if *method == "last" {
                            array.last().at(span)?
                        } else if *method == "at" {
                            array.at(args.expect("index")?, None).at(span)?
                        } else {
                            return missing();
                        }
                    }
                    Value::Dict(dict) => {
                        if *method == "at" {
                            dict.at(args.expect("key")?, None).at(span)?
                        } else {
                            return missing();
                        }
                    }
                    _ => return missing(),
                };

                Ok(Cow::Owned(accessed))
            }
        }
    }

    /// Gets the value using write access.
    pub fn write<'a: 'b, 'b>(
        &self,
        span: Span,
        vm: &'b mut Vm<'a, '_>,
        engine: &mut Engine,
    ) -> SourceResult<&'b mut Value> {
        match self {
            CompiledAccess::Register(reg) => vm.write(*reg).ok_or_else(|| {
                eco_vec![error!(span, "cannot write to a temporary value")]
            }),
            CompiledAccess::Module(_) => {
                bail!(span, "cannot write to a global, malformed access")
            }
            CompiledAccess::Func(_) => {
                bail!(span, "cannot write to a function, malformed access")
            }
            CompiledAccess::Value(_) => {
                bail!(span, "cannot write to a static value, malformed access")
            }
            CompiledAccess::Type(_) => {
                bail!(span, "cannot write to a type, malformed access")
            }
            CompiledAccess::Chained(parent_span, value, field, field_span) => {
                let access = vm.read(*value);
                let value = access.write(span, vm, engine)?;
                match value {
                    Value::Dict(dict) => dict.at_mut(field).at(*field_span),
                    value => {
                        let ty = value.ty();
                        if matches!(
                            value, // those types have their own field getters
                            Value::Symbol(_)
                                | Value::Content(_)
                                | Value::Module(_)
                                | Value::Func(_)
                        ) {
                            bail!(*parent_span, "cannot mutate fields on {ty}");
                        } else if crate::foundations::fields_on(ty).is_empty() {
                            bail!(*parent_span, "{ty} does not have accessible fields");
                        } else {
                            // type supports static fields, which don't yet have
                            // setters
                            bail!(
                                *parent_span,
                                "fields on {ty} are not yet mutable";
                                hint: "try creating a new {ty} with the updated field value instead"
                            )
                        }
                    }
                }
            }
            CompiledAccess::AccessorMethod(value, method, args) => {
                // Get the arguments.
                let args = match *args {
                    Readable::Reg(reg) => vm.take(reg).into_owned(),
                    other => vm.read(other).clone(),
                };

                let args = match args {
                    Value::Args(args) => args.clone(),
                    Value::None => Args::with_capacity(span, 0),
                    _ => bail!(
                        span,
                        "expected argumentss, found {}",
                        args.ty().long_name()
                    ),
                };

                // Get the callee.
                let access = vm.read(*value);
                let value = access.write(span, vm, engine)?;

                let point = || Tracepoint::Call(Some((*method).into()));
                call_method_access(value, method, args, span).trace(
                    engine.world,
                    point,
                    span,
                )
            }
        }
    }
}
*/
