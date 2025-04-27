use ecow::{eco_format, EcoString};
use typst_library::diag::{
    bail, error, At, HintedString, SourceDiagnostic, SourceResult, Trace, Tracepoint,
};
use typst_library::foundations::{
    Arg, Args, Content, Func, NativeElement, Str, SymbolElem, Type, Value,
};
use typst_library::math::LrElem;
use typst_syntax::{Span, Spanned};

use super::flow::Iterable;
use super::{Instruction, MutAccess, Readable, Vm};

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Hash)]
    struct CallMode: u32 {
        const MATH           = 0b0001;
        const TRAILING_COMMA = 0b0010;
    }
}

impl CallMode {
    pub fn new(in_math: bool, trailing_comma: bool) -> Self {
        let mut out = Self::empty();
        if in_math {
            out |= Self::MATH;
        }

        if trailing_comma {
            out |= Self::TRAILING_COMMA;
        }

        out
    }
}

/// Reverses the order of the last `len` elements of the stack.
///
/// Used for preserving the order of call sites.
#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Reverse {
    len: usize,
    span: Span,
}

impl Reverse {
    pub fn new(len: usize, span: Span) -> Self {
        Self { len, span }
    }
}

impl Instruction for Reverse {
    type Output = ();

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        let len = vm.stack.len();

        // Ensure n is not greater than the vector length
        // and handle cases where n or len is 0.
        if self.len < 2 || len == 0 {
            return Ok(());
        }

        // Calculate the starting index of the slice to reverse.
        // Use saturating_sub to prevent panic if n > len.
        // If n >= len, start_index will be 0, reversing the whole vector.
        let start_index = len.saturating_sub(self.len);

        // Get a mutable slice of the last n (or fewer) elements.
        let slice_to_reverse = &mut vm.stack[start_index..];

        // Reverse the slice in place.
        slice_to_reverse.reverse();

        Ok(())
    }

    fn take_slot(&mut self, _: usize) {}
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ArgSegment {
    Single(Span, Span, Readable),
    Named(Span, Span, Str, Readable),
    Spread(Span, Readable),
}

impl ArgSegment {
    pub fn take_slot(&mut self, slot: usize) {
        match self {
            Self::Single(_, _, readable) => readable.take_slot(slot),
            Self::Named(_, _, _, readable) => readable.take_slot(slot),
            Self::Spread(_, readable) => readable.take_slot(slot),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FuncCall(Box<FuncCallRepr>);

#[derive(Debug, Clone, PartialEq, Hash)]
struct FuncCallRepr {
    access: Readable,
    args: Vec<ArgSegment>,
    shadowed: Option<EcoString>,
    full_span: Span,
    target_span: Span,
    args_span: Span,
    mode: CallMode,
}

impl FuncCall {
    pub fn new(
        access: Readable,
        args: Vec<ArgSegment>,
        shadowed: Option<EcoString>,
        full_span: Span,
        target_span: Span,
        args_span: Span,
        in_math: bool,
        trailing_comma: bool,
    ) -> Self {
        Self(Box::new(FuncCallRepr {
            access,
            args,
            shadowed,
            full_span,
            target_span,
            args_span,
            mode: CallMode::new(in_math, trailing_comma),
        }))
    }
}

impl Instruction for FuncCall {
    type Output = Value;

    fn eval(
        &self,
        vm: &mut Vm,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        self.0.eval(vm, iterator)
    }

    fn take_slot(&mut self, slot: usize) {
        self.0.take_slot(slot);
    }
}

impl Instruction for FuncCallRepr {
    type Output = Value;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        vm.engine.route.check_call_depth().at(self.full_span)?;

        let callee = vm.get(self.access, self.target_span)?;
        let args = make_args(vm, &self.args)?;

        // TODO: remove this clone, use references
        let func_result = callee.clone().cast::<Func>();
        if self.mode.contains(CallMode::MATH) && func_result.is_err() {
            return wrap_args_in_math(
                callee,
                self.target_span,
                args,
                self.mode.contains(CallMode::TRAILING_COMMA),
            );
        }

        let func = func_result
            .map_err(|err| hint_if_shadowed_std(self.shadowed.as_ref(), err))
            .at(self.target_span)?;

        let f = move || {
            let point = || Tracepoint::Call(func.name().map(Into::into));
            func.call(&mut vm.engine, vm.context, args).trace(
                vm.world(),
                point,
                self.full_span,
            )
        };

        // Stacker is broken on WASM.
        #[cfg(target_arch = "wasm32")]
        return f();

        #[cfg(not(target_arch = "wasm32"))]
        stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, f)
    }

    fn take_slot(&mut self, slot: usize) {
        self.access.take_slot(slot);
        self.args.iter_mut().for_each(|arg| arg.take_slot(slot));
    }
}

/// Provide a hint if the callee is a shadowed standard library function.
fn hint_if_shadowed_std(
    shadowed: Option<&EcoString>,
    mut err: HintedString,
) -> HintedString {
    if let Some(ident) = shadowed {
        err.hint(eco_format!(
            "use `std.{ident}` to access the shadowed standard library function",
        ));
    }
    err
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct MethodCall(Box<MethodCallRepr>);

#[derive(Debug, Clone, PartialEq, Hash)]
struct MethodCallRepr {
    access: Readable,
    args: Vec<ArgSegment>,
    full_span: Span,
    target_span: Span,
    field_span: Span,
    args_span: Span,
    field: EcoString,
    mode: CallMode,
}

impl MethodCall {
    pub fn new(
        access: Readable,
        args: Vec<ArgSegment>,
        full_span: Span,
        target_span: Span,
        field_span: Span,
        args_span: Span,
        field: EcoString,
        in_math: bool,
        trailing_comma: bool,
    ) -> Self {
        Self(Box::new(MethodCallRepr {
            access,
            args,
            full_span,
            target_span,
            field_span,
            args_span,
            field,
            mode: CallMode::new(in_math, trailing_comma),
        }))
    }
}

impl Instruction for MethodCall {
    type Output = Value;

    fn eval(
        &self,
        vm: &mut Vm,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        self.0.eval(vm, iterator)
    }

    fn take_slot(&mut self, slot: usize) {
        self.0.take_slot(slot);
    }
}

impl Instruction for MethodCallRepr {
    type Output = Value;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        vm.engine.route.check_call_depth().at(self.full_span)?;

        let args = make_args(vm, &self.args);
        let target = vm.get(self.access, self.target_span)?;

        let mut args = args?;
        let callee = get_method(
            vm,
            target,
            &mut args,
            &self.field,
            self.target_span,
            self.field_span,
        )?;

        let func_result = callee.clone().cast::<Func>();
        if self.mode.contains(CallMode::MATH) && func_result.is_err() {
            return wrap_args_in_math(
                callee,
                self.target_span,
                args,
                self.mode.contains(CallMode::TRAILING_COMMA),
            );
        }

        let func = func_result.at(self.target_span)?;

        let f = move || {
            let point = || Tracepoint::Call(func.name().map(Into::into));
            func.call(&mut vm.engine, vm.context, args).trace(
                vm.world(),
                point,
                self.full_span,
            )
        };

        // Stacker is broken on WASM.
        #[cfg(target_arch = "wasm32")]
        return f();

        #[cfg(not(target_arch = "wasm32"))]
        stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, f)
    }

    fn take_slot(&mut self, slot: usize) {
        self.access.take_slot(slot);
        self.args.iter_mut().for_each(|arg| arg.take_slot(slot));
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct MutMethodCall(Box<MutMethodCallRepr>);

#[derive(Debug, Clone, PartialEq, Hash)]
struct MutMethodCallRepr {
    access: MutAccess,
    args: Vec<ArgSegment>,
    full_span: Span,
    field_span: Span,
    args_span: Span,
    field: EcoString,
    mode: CallMode,
}

enum CalleeResolution {
    Done(Value),
    Callee(Args, Value),
}

impl MutMethodCall {
    pub fn new(
        access: MutAccess,
        args: Vec<ArgSegment>,
        full_span: Span,
        field_span: Span,
        args_span: Span,
        field: EcoString,
        in_math: bool,
        trailing_comma: bool,
    ) -> Self {
        Self(Box::new(MutMethodCallRepr {
            access,
            args,
            full_span,
            field_span,
            args_span,
            field,
            mode: CallMode::new(in_math, trailing_comma),
        }))
    }
}

impl Instruction for MutMethodCall {
    type Output = Value;

    fn eval(
        &self,
        vm: &mut Vm,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<Self::Output> {
        self.0.eval(vm, iterator)
    }

    fn take_slot(&mut self, slot: usize) {
        self.0.take_slot(slot);
    }
}

impl Instruction for MutMethodCallRepr {
    type Output = Value;

    fn eval(&self, vm: &mut Vm, _: Option<&mut Iterable>) -> SourceResult<Self::Output> {
        vm.engine.route.check_call_depth().at(self.full_span)?;

        let args = make_args(vm, &self.args);
        let out = vm.access(&self.access, |vm, value| match value {
            target @ (Value::Array(_) | Value::Dict(_)) => {
                let value = call_method_mut(target, &self.field, args?, self.full_span);
                let point = || Tracepoint::Call(Some(self.field.clone()));
                return Ok(CalleeResolution::Done(value.trace(
                    vm.world(),
                    point,
                    self.full_span,
                )?));
            }
            _ => Ok(CalleeResolution::Callee(args?, value.clone())),
        })?;

        // If we did resolve the value via a mut call then we can just return.
        // Otherwise we resume with the "normal" call flow.
        let (mut args, target) = match out {
            CalleeResolution::Done(value) => return Ok(value),
            CalleeResolution::Callee(args, value) => (args, value),
        };

        let callee = get_method(
            vm,
            target,
            &mut args,
            &self.field,
            self.access.span,
            self.field_span,
        )?;

        let func_result = callee.clone().cast::<Func>();
        if self.mode.contains(CallMode::MATH) && func_result.is_err() {
            return wrap_args_in_math(
                callee,
                self.access.span,
                args,
                self.mode.contains(CallMode::TRAILING_COMMA),
            );
        }

        let func = func_result.at(self.access.span)?;

        let f = move || {
            let point = || Tracepoint::Call(func.name().map(Into::into));
            func.call(&mut vm.engine, vm.context, args).trace(
                vm.world(),
                point,
                self.full_span,
            )
        };

        // Stacker is broken on WASM.
        #[cfg(target_arch = "wasm32")]
        return f();

        #[cfg(not(target_arch = "wasm32"))]
        stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, f)
    }

    fn take_slot(&mut self, slot: usize) {
        self.args.iter_mut().for_each(|arg| arg.take_slot(slot));
    }
}

pub(crate) fn make_args(vm: &mut Vm, segments: &[ArgSegment]) -> SourceResult<Args> {
    // We do *not* use the `self.span` here because we want the callsite
    // span to be one level higher (the whole function call).
    let mut args = Args::with_capacity(Span::detached(), segments.len());
    for segment in segments {
        match segment {
            ArgSegment::Single(span, value_span, readable) => {
                let value = vm.get(*readable, *span)?;
                args.push(*span, *value_span, value);
            }
            ArgSegment::Named(span, value_span, name, readable) => {
                let value = vm.get(*readable, *span)?;
                args.push_named(*span, *value_span, name.clone(), value);
            }
            ArgSegment::Spread(span, readable) => {
                let value = vm.get(*readable, *span)?;
                match value {
                    Value::None => {}
                    Value::Array(array) => {
                        args.items.extend(array.into_iter().map(|value| Arg {
                            span: *span,
                            name: None,
                            value: Spanned::new(value, *span),
                        }));
                    }
                    Value::Dict(dict) => {
                        args.items.extend(dict.into_iter().map(|(key, value)| Arg {
                            span: *span,
                            name: Some(key),
                            value: Spanned::new(value, *span),
                        }));
                    }
                    Value::Args(other) => args.items.extend(other.items),
                    v => bail!(*span, "cannot spread {}", v.ty()),
                }
            }
        }
    }

    Ok(args)
}

pub(crate) fn get_method(
    vm: &mut Vm,
    target: Value,
    args: &mut Args,
    field: &str,
    target_span: Span,
    field_span: Span,
) -> SourceResult<Value> {
    let sink = (&mut vm.engine, field_span);
    if let Some(callee) = target.ty().scope().get(&field) {
        args.insert(0, target_span, target);
        Ok(callee.read_checked(sink).clone())
    } else if let Value::Content(content) = &target {
        if let Some(callee) = content.elem().scope().get(&field) {
            args.insert(0, target_span, target);
            Ok(callee.read_checked(sink).clone())
        } else {
            bail!(missing_field_call_error(target, field, field_span))
        }
    } else if matches!(
        target,
        Value::Symbol(_) | Value::Func(_) | Value::Type(_) | Value::Module(_)
    ) {
        // Certain value types may have their own ways to access method fields.
        // e.g. `$arrow.r(v)$`, `table.cell[..]`
        let value = target.field(&field, sink).at(field_span)?;
        Ok(value)
    } else {
        // Otherwise we cannot call this field.
        bail!(missing_field_call_error(target, field, field_span))
    }
}

/// Call a mutating method on a value.
pub(crate) fn call_method_mut(
    value: &mut Value,
    method: &str,
    mut args: Args,
    span: Span,
) -> SourceResult<Value> {
    let ty = value.ty();
    let missing = || Err(missing_method(ty, method)).at(span);
    let mut output = Value::None;

    match value {
        Value::Array(array) => match method {
            "push" => array.push(args.expect("value")?),
            "pop" => output = array.pop().at(span)?,
            "insert" => {
                array.insert(args.expect("index")?, args.expect("value")?).at(span)?
            }
            "remove" => {
                output = array
                    .remove(args.expect("index")?, args.named("default")?)
                    .at(span)?
            }
            _ => return missing(),
        },

        Value::Dict(dict) => match method {
            "insert" => dict.insert(args.expect::<Str>("key")?, args.expect("value")?),
            "remove" => {
                output =
                    dict.remove(args.expect("key")?, args.named("default")?).at(span)?
            }
            _ => return missing(),
        },

        _ => return missing(),
    }

    args.finish()?;
    Ok(output)
}

/// For non-functions in math, we wrap the arguments in parentheses.
#[cold]
fn wrap_args_in_math(
    callee: Value,
    callee_span: Span,
    mut args: Args,
    trailing_comma: bool,
) -> SourceResult<Value> {
    let mut body = Content::empty();
    for (i, arg) in args.all::<Content>()?.into_iter().enumerate() {
        if i > 0 {
            body += SymbolElem::packed(',');
        }
        body += arg;
    }
    if trailing_comma {
        body += SymbolElem::packed(',');
    }
    Ok(Value::Content(
        callee.display().spanned(callee_span)
            + LrElem::new(SymbolElem::packed('(') + body + SymbolElem::packed(')'))
                .pack()
                .spanned(args.span),
    ))
}

/// The missing method error message.
#[cold]
fn missing_method(ty: Type, method: &str) -> String {
    format!("type {ty} has no method `{method}`")
}

/// Produce an error when we cannot call the field.
#[cold]
fn missing_field_call_error(
    target: Value,
    field: &str,
    field_span: Span,
) -> SourceDiagnostic {
    let mut error = match &target {
        Value::Content(content) => error!(
            field_span,
            "element {} has no method `{field}`",
            content.elem().name(),
        ),
        _ => error!(field_span, "type {} has no method `{field}`", target.ty(),),
    };

    match target {
        Value::Dict(ref dict) if matches!(dict.get(&field), Ok(Value::Func(_))) => {
            error.hint(eco_format!(
                "to call the function stored in the dictionary, surround \
                the field access with parentheses, e.g. `(dict.{field})(..)`",
            ));
        }
        _ if target.field(&field, ()).is_ok() => {
            error.hint(eco_format!("did you mean to access the field `{field}`?",));
        }
        _ => {}
    }

    error
}
