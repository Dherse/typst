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
    call_method_access, call_method_mut, missing_method, Arg, Args, Bytes, Content,
    Context, Func, IntoValue, NativeElement, Str, Value,
};
use crate::lang::compiled::{CompiledAccess, CompiledAccessRoot, CompiledAccessSegment};
use crate::lang::interpreter::methods::ValueAccessor;
use crate::lang::operands::Readable;
use crate::math::LrElem;
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
            parent: AccessedValue,
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
                    let args = match args.pop().unwrap() {
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
                                engine,
                                context,
                                mut_,
                                *name,
                                args,
                                *span,
                                *name_span,
                                is_last && in_math,
                                *trailing_comma,
                            )
                            .trace(engine.world, point, *span)?
                            .spanned(*span);

                            AccessedValue::Owned(value)
                        }
                        AccessedValue::Borrowed(ref_) => {
                            let point = || Tracepoint::Call(Some((*name).into()));
                            let value = call_method_immutable(
                                engine,
                                context,
                                ref_,
                                *name,
                                args,
                                *span,
                                *name_span,
                                is_last && in_math,
                                *trailing_comma,
                            )
                            .trace(engine.world, point, *span)?
                            .spanned(*span);

                            AccessedValue::Owned(value)
                        }
                        AccessedValue::Owned(owned) => {
                            let point = || Tracepoint::Call(Some((*name).into()));
                            let value = call_method_immutable(
                                engine,
                                context,
                                &owned,
                                *name,
                                args,
                                *span,
                                *name_span,
                                is_last && in_math,
                                *trailing_comma,
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
    in_math: bool,
    trailing_comma: bool,
) -> SourceResult<Value> {
    let ty = value.ty();
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

    // Prioritize associated functions on the value's type (i.e.,
    // methods) over its fields. A function call on a field is only
    // allowed for functions, types, modules (because they are scopes),
    // and symbols (because they have modifiers).
    //
    // For dictionaries, it is not allowed because it would be ambiguous
    // (prioritizing associated functions would make an addition of a
    // new associated function a breaking change and prioritizing fields
    // would break associated functions for certain dictionaries).
    let callee = if let Some(callee) = value.ty().scope().get(method) {
        let this = Arg {
            span,
            name: None,
            value: Spanned::new(value.clone(), span),
        };
        args.items.insert(0, this);

        callee.clone()
    } else if matches!(
        value,
        Value::Symbol(_) | Value::Func(_) | Value::Type(_) | Value::Module(_)
    ) {
        value.field(method).at(name_span)?
    } else {
        let mut error = error!(name_span, "type {} has no method `{method}`", value.ty());

        let mut field_hint = || {
            if value.field(method).is_ok() {
                error.hint(eco_format!("did you mean to access the field `{method}`?",));
            }
        };

        match value {
            Value::Dict(ref dict) => {
                if matches!(dict.get(method), Ok(Value::Func(_))) {
                    error.hint(eco_format!(
                        "to call the function stored in the dictionary, surround \
                        the field access with parentheses, e.g. `(dict.{method})(..)`"
                    ));
                } else {
                    field_hint();
                }
            }
            _ => field_hint(),
        }

        bail!(error);
    };

    let func_result = callee.clone().cast::<Func>().at(name_span);
    if in_math && func_result.is_err() {
        // For non-functions in math, we wrap the arguments in parentheses.
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
        return Ok(Value::Content(
            callee.display().spanned(name_span)
                + LrElem::new(TextElem::packed('(') + body + TextElem::packed(')'))
                    .pack(),
        ));
    }

    // Call the function.
    let func = func_result?;
    let point = || Tracepoint::Call(func.name().map(Into::into));
    let value = func
        .call(engine, context, args)
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

                let func_result = func.clone().cast::<Func>().at(*span);
                if in_math && func_result.is_err() {
                    // For non-functions in math, we wrap the arguments in parentheses.
                    let mut body = Content::empty();
                    for (i, arg) in args.all::<Content>()?.into_iter().enumerate() {
                        if i > 0 {
                            body += TextElem::packed(',');
                        }
                        body += arg;
                    }
                    if *trailing_comma {
                        body += TextElem::packed(',');
                    }
                    return Ok(AccessedValue::Owned(Value::Content(
                        func.clone().display().spanned(*span)
                            + LrElem::new(
                                TextElem::packed('(') + body + TextElem::packed(')'),
                            )
                            .pack(),
                    )));
                }

                let func = func_result?;
                let value = func.call(engine, vm.context, args)?;

                AccessedValue::Owned(value.spanned(*span))
            }
            Self::Value(value) => AccessedValue::Borrowed(value),
        })
    }
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
