use std::iter::Peekable;

use ecow::EcoString;
use typst_library::diag::{bail, At, HintedStrResult, SourceResult};
use typst_library::foundations::{Args, Dict, Str, Type, Value};
use typst_syntax::Span;

use super::call::{make_args, ArgSegment};
use super::{Instruction, Readable, Vm};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FieldAccess {
    pub target: Readable,
    pub target_span: Span,

    pub field: EcoString,
    pub field_span: Span,
}

impl FieldAccess {
    pub fn new(
        target: Readable,
        target_span: Span,
        field: EcoString,
        field_span: Span,
    ) -> Self {
        Self { target, target_span, field, field_span }
    }
}

impl Instruction for FieldAccess {
    type Output = Value;

    fn eval(
        &self,
        vm: &mut Vm,
        _: Option<&mut super::flow::Iterable>,
    ) -> SourceResult<Self::Output> {
        let value = vm.get(self.target, self.target_span)?;

        let err = match value
            .field(&self.field, (&mut vm.engine, self.field_span))
            .at(self.field_span)
        {
            Ok(value) => return Ok(value),
            Err(err) => err,
        };

        // Check whether this is a get rule field access.
        if_chain::if_chain! {
            if let Value::Func(func) = value;
            if let Some(element) = func.element();
            if let Some(id) = element.field_id(&self.field);
            let styles = vm.context.styles().at(self.field_span);
            if let Ok(value) = element.field_from_styles(
                id,
                styles.as_ref().map(|&s| s).unwrap_or_default(),
            );
            then {
                // Only validate the context once we know that this is indeed
                // a field from the style chain.
                let _ = styles?;
                return Ok(value);
            }
        }

        Err(err)
    }

    fn take_slot(&mut self, slot: usize) {
        self.target.take_slot(slot);
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct MutAccess {
    /// The head of a mutable access.
    pub head: usize,
    pub head_name: EcoString,

    pub segments: Vec<AccessSegment>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum AccessSegment {
    Field { name: EcoString, span: Span },
    Call { method: EcoString, span: Span, args: Vec<ArgSegment> },
}

impl MutAccess {
    pub fn eval<O>(
        &self,
        vm: &mut Vm,
        ops: impl FnOnce(&mut Vm, &mut Value) -> SourceResult<O>,
    ) -> SourceResult<O> {
        vm.lock(self.head, self.span, |vm, value| {
            recursive_mut_access(vm, value, self.segments.iter(), ops)
        })
    }

    pub fn set(&self, vm: &mut Vm, to: Value) -> SourceResult<()> {
        vm.lock(self.head, self.span, |vm, value| {
            recursive_set(vm, value, self.segments.iter().peekable(), to)
        })
    }
}

fn recursive_mut_access<'a, O>(
    vm: &mut Vm,
    value: &mut Value,
    mut segments: impl Iterator<Item = &'a AccessSegment>,
    ops: impl FnOnce(&mut Vm, &mut Value) -> SourceResult<O>,
) -> SourceResult<O> {
    let Some(next) = segments.next() else {
        return ops(vm, value);
    };

    match next {
        AccessSegment::Field { name, span } => {
            let dict = access_dict(value).at(*span)?;

            recursive_mut_access(vm, dict.at_mut(name).at(*span)?, segments, ops)
        }
        AccessSegment::Call { method, span, args } => {
            let args = make_args(vm, args)?;
            let next = call_method_access(value, method, args, *span)?;
            recursive_mut_access(vm, next, segments, ops)
        }
    }
}

fn recursive_set<'a, I>(
    vm: &mut Vm,
    value: &mut Value,
    mut segments: Peekable<I>,
    to: Value,
) -> SourceResult<()>
where
    I: Iterator<Item = &'a AccessSegment>,
{
    let Some(next) = segments.next() else {
        *value = to;
        return Ok(());
    };

    match next {
        AccessSegment::Field { name, span } => {
            let dict = access_dict(value).at(*span)?;

            // If we are the last, we directly insert the value (we can create it)
            if segments.next().is_none() {
                dict.insert(name.clone().into(), to);
                return Ok(());
            }

            // Otherwise, we just keep going through the tree
            recursive_set(vm, dict.at_mut(name).at(*span)?, segments, to)
        }
        AccessSegment::Call { method, span, args } => {
            let args = make_args(vm, args)?;
            let next = call_method_access(value, method, args, *span)?;
            recursive_set(vm, next, segments, to)
        }
    }
}

fn call_method_access<'a>(
    value: &'a mut Value,
    method: &str,
    mut args: Args,
    span: Span,
) -> SourceResult<&'a mut Value> {
    let ty = value.ty();
    let missing = || Err(missing_method(ty, method)).at(span);

    let slot = match value {
        Value::Array(array) => match method {
            "first" => array.first_mut().at(span)?,
            "last" => array.last_mut().at(span)?,
            "at" => array.at_mut(args.expect("index")?).at(span)?,
            _ => return missing(),
        },
        Value::Dict(dict) => match method {
            "at" => dict.at_mut(&args.expect::<Str>("key")?).at(span)?,
            _ => return missing(),
        },
        _ => return missing(),
    };

    args.finish()?;
    Ok(slot)
}

fn access_dict(parent: &mut Value) -> HintedStrResult<&mut Dict> {
    match parent {
        Value::Dict(dict) => Ok(dict),
        value => {
            let ty = value.ty();
            if matches!(
                value, // those types have their own field getters
                Value::Symbol(_) | Value::Content(_) | Value::Module(_) | Value::Func(_)
            ) {
                bail!("cannot mutate fields on {ty}");
            } else if typst_library::foundations::fields_on(ty).is_empty() {
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

/// The missing method error message.
#[cold]
fn missing_method(ty: Type, method: &str) -> String {
    format!("type {ty} has no method `{method}`")
}
