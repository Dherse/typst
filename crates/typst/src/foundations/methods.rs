//! Handles special built-in methods on values.

use crate::diag::{bail, At, HintedStrResult, SourceResult};
use crate::foundations::{Args, Array, Dict, Str, Type, Value};
use crate::syntax::Span;

/// List the available methods for a type and whether they take arguments.
pub fn mutable_methods_on(ty: Type) -> &'static [(&'static str, bool)] {
    if ty == Type::of::<Array>() {
        &[
            ("first", false),
            ("last", false),
            ("at", true),
            ("pop", false),
            ("push", true),
            ("insert", true),
            ("remove", true),
        ]
    } else if ty == Type::of::<Dict>() {
        &[("at", true), ("insert", true), ("remove", true)]
    } else {
        &[]
    }
}

/// Whether a specific method is mutating.
pub(crate) fn is_mutating_method(method: &str) -> bool {
    matches!(method, "push" | "pop" | "insert" | "remove")
}

/// Call a mutating method on a value.
pub(crate) fn call_method_mut(
    value: &mut Value,
    method: &str,
    mut args: Args,
    span: Span,
    name_span: Span,
) -> SourceResult<Value> {
    let ty = value.ty();
    let missing = || missing_method(ty, method).at(name_span);
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

/// Call an accessor method on a value.
pub(crate) fn call_method_access<'a>(
    value: &'a mut Value,
    method: &str,
    mut args: Args,
    span: Span,
    name_span: Span,
) -> SourceResult<&'a mut Value> {
    let ty = value.ty();
    let missing = || missing_method(ty, method).at(name_span);

    let slot = match value {
        Value::Array(array) => match method {
            "first" => array.first_mut().at(span)?,
            "last" => array.last_mut().at(span)?,
            "at" => array.at_mut(args.expect("index")?).at(span)?,
            other if Type::of::<Array>().scope().contains(other) => {
                bail!(span, "cannot mutate a temporary value");
            }
            _ => return missing(),
        },
        Value::Dict(dict) => match method {
            "at" => dict.at_mut(&args.expect::<Str>("key")?).at(span)?,
            other if Type::of::<Array>().scope().contains(other) => {
                bail!(span, "cannot mutate a temporary value");
            }
            _ => return missing(),
        },
        _ => return missing(),
    };

    args.finish()?;
    Ok(slot)
}

/// The missing method error message.
#[cold]
pub fn missing_method<T>(ty: Type, method: &str) -> HintedStrResult<T> {
    bail!("type {ty} has no method `{method}`")
}
