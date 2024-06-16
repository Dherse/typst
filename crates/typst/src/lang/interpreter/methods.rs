use comemo::Tracked;
use typst_syntax::Span;
use typst_utils::pico;

use crate::diag::{bail, At, SourceResult};
use crate::engine::Engine;
use crate::foundations::{missing_method, Args, Bytes, Context, IntoValue, Str, Value};

pub trait ValueAccessor {
    fn is_mut(&self, method: &str) -> bool;

    fn is_access_mut(&self, method: &str) -> bool;

    fn call(
        &self,
        engine: &mut Engine,
        context: Tracked<Context>,
        span: Span,
        args: Args,
    ) -> SourceResult<Value>;

    fn method(
        &self,
        engine: &mut Engine,
        context: Tracked<Context>,
        span: Span,
        method: &str,
        args: Args,
    ) -> SourceResult<Value>;

    fn method_mut(
        &mut self,
        span: Span,
        method: &str,
        args: Args,
        value: Value,
    ) -> SourceResult<Value>;
}

impl ValueAccessor for Value {
    fn is_access_mut(&self, method: &str) -> bool {
        match self {
            Value::Array(_) => matches!(method, "at" | "first" | "last"),
            Value::Dict(_) => matches!(method, "at"),
            _ => false,
        }
    }

    fn is_mut(&self, method: &str) -> bool {
        match self {
            Value::Array(_) => matches!(method, "push" | "pop" | "insert" | "remove"),
            Value::Dict(_) => matches!(method, "insert" | "remove"),
            _ => false,
        }
    }

    fn call(
        &self,
        engine: &mut Engine,
        context: Tracked<Context>,
        span: Span,
        args: Args,
    ) -> SourceResult<Value> {
        match self {
            Self::Func(func) => func.call(engine, context, args),
            Self::Type(ty) => ty.constructor().at(span)?.call(engine, context, args),
            _ => bail!(span, "expected function, found {}", self.ty()),
        }
    }

    fn method(
        &self,
        engine: &mut Engine,
        context: Tracked<Context>,
        span: Span,
        method: &str,
        mut args: Args,
    ) -> SourceResult<Value> {
        match self {
            Self::Plugin(plugin) => {
                let bytes = args.all::<Bytes>()?;
                args.finish()?;

                plugin.call(method, bytes).at(span).map(IntoValue::into_value)
            }
            other => other.field(method).at(span)?.call(engine, context, span, args),
        }
    }

    fn method_mut(
        &mut self,
        span: Span,
        method: &str,
        mut args: Args,
        value: Value,
    ) -> SourceResult<Value> {
        let ty = value.ty();
        let missing = || missing_method(ty, method).at(span);
        let mut output: Value = Value::None;

        match self {
            Value::Array(array) => match method {
                "push" => array.push(args.expect(pico!("value"))?),
                "pop" => output = array.pop().at(span)?,
                "insert" => array
                    .insert(args.expect(pico!("index"))?, args.expect(pico!("value"))?)
                    .at(span)?,
                "remove" => {
                    output = array
                        .remove(
                            args.expect(pico!("index"))?,
                            args.named(pico!("default"))?,
                        )
                        .at(span)?
                }
                _ => return missing(),
            },

            Value::Dict(dict) => match method {
                "insert" => dict.insert(
                    args.expect::<Str>(pico!("key"))?,
                    args.expect(pico!("value"))?,
                ),
                "remove" => {
                    output = dict
                        .remove(args.expect(pico!("key"))?, args.named(pico!("default"))?)
                        .at(span)?
                }
                _ => return missing(),
            },

            _ => return missing(),
        }

        Ok(output)
    }
}
