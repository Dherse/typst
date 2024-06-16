use std::collections::HashSet;

use typst_syntax::Span;

use crate::diag::{bail, At, SourceResult};
use crate::engine::Engine;
use crate::foundations::{Array, Dict, IntoValue, Value};
use crate::lang::compiled::{CompiledPattern, CompiledPatternItem, CompiledPatternKind};

use super::run::IteratorValue;
use super::Vm;

impl CompiledPattern {
    pub fn write(
        &self,
        vm: &mut Vm,
        engine: &mut Engine,
        value: &Value,
    ) -> SourceResult<()> {
        match &self.kind {
            CompiledPatternKind::Single(single) => match single {
                // Placeholders simply discards the value.
                CompiledPatternItem::Placeholder(_) => {}
                CompiledPatternItem::Simple(span, local_id, _) => {
                    let access = vm.read(*local_id);
                    access.get(vm, engine, self.declare, |accessor| {
                        accessor.write(value.clone()).at(*span)
                    })?;
                }
                CompiledPatternItem::Named(span, _, _) => bail!(
                    *span,
                    "cannot destructure {} with named pattern",
                    value.ty().long_name()
                ),
                CompiledPatternItem::Spread(span, _)
                | CompiledPatternItem::SpreadDiscard(span) => bail!(
                    *span,
                    "cannot destructure {} with spread",
                    value.ty().long_name()
                ),
                CompiledPatternItem::Nested(_, pattern_id) => {
                    let pattern = vm.read(*pattern_id);
                    pattern.write(vm, engine, value)?;
                }
            },
            CompiledPatternKind::Tuple(tuple, span, has_sink) => match value {
                Value::Array(array) => destructure_array(
                    vm,
                    engine,
                    array.as_slice(),
                    tuple,
                    *span,
                    self.declare,
                )?,
                Value::Dict(dict) => {
                    destructure_dict(vm, engine, dict, *has_sink, self.declare, tuple)?
                }
                other => {
                    bail!(self.span, "cannot destructure {}", other.ty().long_name())
                }
            },
        }

        Ok(())
    }

    pub fn write_itered(
        &self,
        vm: &mut Vm,
        engine: &mut Engine,
        value: IteratorValue,
    ) -> SourceResult<()> {
        match &self.kind {
            CompiledPatternKind::Single(single) => match single {
                // Placeholders simply discards the value.
                CompiledPatternItem::Placeholder(_) => {}
                CompiledPatternItem::Simple(span, local_id, _) => {
                    let access = vm.read(*local_id);
                    access.get(vm, engine, self.declare, |accessor| {
                        accessor.write(value.into_value()).at(*span)
                    })?;
                }
                CompiledPatternItem::Named(span, _, _) => bail!(
                    *span,
                    "cannot destructure {} with named pattern",
                    value.type_name()
                ),
                CompiledPatternItem::Spread(span, _)
                | CompiledPatternItem::SpreadDiscard(span) => {
                    bail!(*span, "cannot destructure {} with spread", value.type_name())
                }
                CompiledPatternItem::Nested(_, pattern_id) => {
                    let pattern = vm.read(*pattern_id);
                    match value {
                        IteratorValue::Array(value) => {
                            pattern.write(vm, engine, value)?
                        }
                        _ => pattern.write(vm, engine, &value.into_value())?,
                    }
                }
            },
            CompiledPatternKind::Tuple(tuple, span, ..) => match value {
                IteratorValue::Dict(key, value) => {
                    let array = [key.clone().into_value(), value.clone()];
                    destructure_array(vm, engine, &array, tuple, *span, self.declare)?
                }
                // Optimization: used borrowed value to avoid cloning.
                IteratorValue::Array(value) => self.write(vm, engine, value)?,
                IteratorValue::Bytes(_) => bail!(*span, "cannot destructure bytes"),
                _ => self.write(vm, engine, &value.into_value())?,
            },
        }

        Ok(())
    }
}

/// Perform destructuring on an array.
fn destructure_array(
    vm: &mut Vm,
    engine: &mut Engine,
    slice: &[Value],
    tuple: &[CompiledPatternItem],
    span: Span,
    declare: bool,
) -> SourceResult<()> {
    let mut i = 0;
    let len = slice.len();

    let check_len = |i: usize, span: Span| {
        if i < len {
            Ok(())
        } else {
            bail!(span, "not enough elements to destructure")
        }
    };

    for p in tuple {
        match p {
            CompiledPatternItem::Placeholder(span) => {
                check_len(i, *span)?;
            }
            CompiledPatternItem::Simple(span, access, _) => {
                check_len(i, *span)?;

                // Resolve the access and write the value.
                let access = vm.read(*access);
                access.get(vm, engine, declare, |accessor| {
                    accessor.write(slice[i].clone()).at(*span)
                })?;

                i += 1;
            }
            CompiledPatternItem::Nested(span, nested_id) => {
                check_len(i, *span)?;

                let nested = vm.read(*nested_id);
                nested.write(vm, engine, &slice[i])?;
                i += 1;
            }
            CompiledPatternItem::Spread(span, access_id) => {
                let sink_size = (1 + len).checked_sub(tuple.len());
                let sink = sink_size.and_then(|s| slice.get(i..i + s));

                if let (Some(sink_size), Some(sink)) = (sink_size, sink) {
                    let access = vm.read(*access_id);
                    access.get(vm, engine, declare, |accessor| {
                        accessor.write(Value::Array(Array::from(sink))).at(*span)
                    })?;
                    i += sink_size;
                } else {
                    bail!(*span, "not enough elements to destructure")
                }
            }
            CompiledPatternItem::SpreadDiscard(span) => {
                let sink_size = (1 + len).checked_sub(tuple.len());
                let sink = sink_size.and_then(|s| slice.get(i..i + s));
                if let (Some(sink_size), Some(_)) = (sink_size, sink) {
                    i += sink_size;
                } else {
                    bail!(*span, "not enough elements to destructure")
                }
            }
            CompiledPatternItem::Named(span, _, _) => {
                bail!(*span, "cannot destructure named pattern from an array")
            }
        }
    }

    if i < len {
        bail!(span, "too many elements to destructure");
    }

    Ok(())
}

fn destructure_dict(
    vm: &mut Vm,
    engine: &mut Engine,
    dict: &Dict,
    has_sink: bool,
    declare: bool,
    tuple: &[CompiledPatternItem],
) -> SourceResult<()> {
    // If there is no sink, we purposefully don't bother allocating
    // a set for the used keys.
    let mut sink = None;
    let mut used = has_sink.then(HashSet::new);

    for p in tuple {
        match p {
            CompiledPatternItem::Placeholder(_) => {}
            CompiledPatternItem::Simple(span, local, key) => {
                let key = key.resolve();
                let v = dict.get(key).at(*span)?;

                let access = vm.read(*local);
                access.get(vm, engine, declare, |accessor| {
                    accessor.write(v.clone()).at(*span)
                })?;

                used.as_mut().map(|u| u.insert(key));
            }
            CompiledPatternItem::Nested(span, _) => {
                bail!(*span, "cannot destructure unnamed pattern from dictionary");
            }
            CompiledPatternItem::Spread(span, local) => sink = Some((*span, Some(local))),
            CompiledPatternItem::SpreadDiscard(span) => sink = Some((*span, None)),
            CompiledPatternItem::Named(span, local, key) => {
                let key = key.resolve();
                let v = dict.get(key).at(*span)?;

                let access = vm.read(*local);
                access.get(vm, engine, declare, |accessor| {
                    accessor.write(v.clone()).at(*span)
                })?;

                used.as_mut().map(|u| u.insert(key));
            }
        }
    }

    if let Some((span, local)) = sink {
        let used = used.unwrap_or_else(|| HashSet::with_capacity(0));
        if let Some(local) = local {
            let mut sink = Dict::with_capacity(dict.len().saturating_sub(used.len()));
            for (key, value) in dict {
                if !used.is_empty() && !used.contains(key.as_str()) {
                    sink.insert(key.clone(), value.clone());
                }
            }

            let access = vm.read(*local);
            access.get(vm, engine, declare, |accessor| {
                accessor.write(Value::Dict(sink)).at(span)
            })?;
        }
    }

    Ok(())
}
