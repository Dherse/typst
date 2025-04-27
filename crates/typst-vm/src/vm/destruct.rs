use ecow::eco_format;
use typst_library::diag::{bail, error, At, SourceDiagnostic, SourceResult};
use typst_library::foundations::{Array, Dict, IntoValue, Str, Value};
use typst_syntax::Span;

use super::access::MutAccess;
use super::{Instruction, Readable, Vm};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct Write {
    value: Readable,
    slot: usize,
    span: Span,
}

impl Write {
    pub fn new(value: Readable, slot: usize, span: Span) -> Self {
        Self { value, slot, span }
    }
}

impl Instruction for Write {
    type Output = ();

    fn eval(
        &self,
        vm: &mut Vm,
        _: Option<&mut super::flow::Iterable>,
    ) -> SourceResult<Self::Output> {
        let value = vm.get(self.value, self.span)?;
        vm.slots[self.slot] = value;
        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.value.take_slot(slot);
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Destructure {
    value: Readable,
    pattern: Pattern,
    span: Span,
}

impl Destructure {
    pub fn new(value: Readable, pattern: Pattern, span: Span) -> Self {
        Self { value, pattern, span }
    }
}

impl Instruction for Destructure {
    type Output = ();

    fn eval(
        &self,
        vm: &mut Vm,
        _: Option<&mut super::flow::Iterable>,
    ) -> SourceResult<Self::Output> {
        let value = vm.get(self.value, self.span)?;

        self.pattern.write(vm, value)?;

        Ok(())
    }

    fn take_slot(&mut self, slot: usize) {
        self.value.take_slot(slot);
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Pattern {
    Single(MutAccess),
    Placeholder(Span),
    Items(Vec<PatternItem>, Span),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Self::Single(access) => access.span,
            Self::Placeholder(span) => *span,
            Self::Items(_, span) => *span,
        }
    }

    pub fn write(&self, vm: &mut Vm, value: Value) -> SourceResult<()> {
        match self {
            Self::Single(single) => single.eval(vm, |_vm, out| {
                *out = value;
                Ok(())
            })?,
            Self::Placeholder(_) => {}
            Self::Items(items, span) => match value {
                Value::Array(array) => {
                    destructure_array(vm, array.into_iter(), items, *span)?
                }
                Value::Dict(dict) => destructure_dict(vm, dict, items)?,
                other => {
                    bail!(*span, "cannot destructure {}", other.ty())
                }
            },
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PatternItem {
    Spread(MutAccess),
    SpreadDiscard(Span),
    Placeholder(Span),
    PlaceholderNamed(Span, Str),
    Named(MutAccess, Str),
    Nested(Box<Pattern>),
    NestedNamed(Box<Pattern>, Str),
    Single(MutAccess),
}

/// Perform destructuring on an array.
fn destructure_array<I: ExactSizeIterator<Item = Value>>(
    vm: &mut Vm,
    mut items: I,
    tuple: &[PatternItem],
    span: Span,
) -> SourceResult<()> {
    let len = items.len();

    let next = |items: &mut I, span: Span| {
        if let Some(item) = items.next() {
            Ok(item)
        } else {
            bail!(wrong_number_of_elements(
                tuple
                    .iter()
                    .filter(|m| !matches!(
                        m,
                        PatternItem::Spread(_) | PatternItem::SpreadDiscard(_)
                    ))
                    .count(),
                tuple.iter().any(|m| matches!(
                    m,
                    PatternItem::Spread(_) | PatternItem::SpreadDiscard(_)
                )),
                span,
                len
            ));
        }
    };

    for p in tuple {
        match p {
            PatternItem::Placeholder(_) => {
                next(&mut items, span)?;
            }
            PatternItem::Single(access) => {
                let item = next(&mut items, span)?;

                // Resolve the access and write the value.
                access.eval(vm, |_vm, out| {
                    *out = item;
                    Ok(())
                })?;
            }
            PatternItem::Nested(nested) => {
                let item = next(&mut items, span)?;

                nested.write(vm, item)?;
            }
            PatternItem::Spread(access) => {
                let sink_size = (1 + len).checked_sub(tuple.len());
                let sink = sink_size.map(|s| (&mut items).take(s));

                if let Some(sink) = sink {
                    access.eval(vm, |_vm, out| {
                        *out = sink.collect::<Array>().into_value();

                        Ok(())
                    })?;
                } else {
                    bail!(wrong_number_of_elements(
                        tuple
                            .iter()
                            .filter(|m| !matches!(
                                m,
                                PatternItem::Spread(_) | PatternItem::SpreadDiscard(_)
                            ))
                            .count(),
                        tuple.iter().any(|m| matches!(
                            m,
                            PatternItem::Spread(_) | PatternItem::SpreadDiscard(_)
                        )),
                        span,
                        len
                    ));
                }
            }
            PatternItem::SpreadDiscard(_) => {
                let sink_size = (1 + len).checked_sub(tuple.len());
                let sink = sink_size.map(|s| (&mut items).take(s));

                if let Some(sink) = sink {
                    sink.for_each(drop)
                } else {
                    bail!(wrong_number_of_elements(
                        tuple
                            .iter()
                            .filter(|m| !matches!(
                                m,
                                PatternItem::Spread(_) | PatternItem::SpreadDiscard(_)
                            ))
                            .count(),
                        tuple.iter().any(|m| matches!(
                            m,
                            PatternItem::Spread(_) | PatternItem::SpreadDiscard(_)
                        )),
                        span,
                        len
                    ));
                }
            }
            PatternItem::Named(access, _) => {
                bail!(access.span, "cannot destructure named pattern from an array")
            }
            PatternItem::PlaceholderNamed(span, _) => {
                bail!(*span, "cannot destructure named pattern from an array")
            }
            PatternItem::NestedNamed(pattern, _) => {
                bail!(pattern.span(), "cannot destructure named pattern from an array")
            }
        }
    }

    if items.next().is_some() {
        bail!(wrong_number_of_elements(
            tuple
                .iter()
                .filter(|m| !matches!(
                    m,
                    PatternItem::Spread(_) | PatternItem::SpreadDiscard(_)
                ))
                .count(),
            tuple.iter().any(|m| matches!(
                m,
                PatternItem::Spread(_) | PatternItem::SpreadDiscard(_)
            )),
            span,
            len
        ));
    }

    Ok(())
}

fn destructure_dict(
    vm: &mut Vm,
    mut dict: Dict,
    tuple: &[PatternItem],
) -> SourceResult<()> {
    let mut sink = None;

    for p in tuple {
        match p {
            PatternItem::Placeholder(_) => {}
            PatternItem::Named(access, name) => {
                access.eval(vm, |_vm, out| {
                    *out = dict.remove(name.clone(), None).at(access.span)?;
                    Ok(())
                })?;
            }
            PatternItem::PlaceholderNamed(span, name) => {
                dict.remove(name.clone(), None).at(*span)?;
            }
            PatternItem::NestedNamed(pattern, name) => {
                pattern.write(vm, dict.remove(name.clone(), None).at(pattern.span())?)?;
            }
            PatternItem::Nested(nested) => {
                bail!(
                    nested.span(),
                    "cannot destructure unnamed pattern from dictionary"
                );
            }
            PatternItem::Single(access) => {
                access.eval(vm, |_vm, out| {
                    *out = dict
                        .remove(access.head_name.clone().into(), None)
                        .at(access.span)?;
                    Ok(())
                })?;
            }
            PatternItem::Spread(access) => {
                sink = Some(access);
            }
            PatternItem::SpreadDiscard(_) => {}
        }
    }

    if let Some(access) = sink {
        access.eval(vm, |_vm, out| {
            *out = dict.into_value();
            Ok(())
        })?;
    }

    Ok(())
}

/// The error message when the number of elements of the destructuring and the
/// array is mismatched.
#[cold]
fn wrong_number_of_elements(
    count: usize,
    spread: bool,
    span: Span,
    len: usize,
) -> SourceDiagnostic {
    let quantifier = if len > count { "too many" } else { "not enough" };
    let expected = match (spread, count) {
        (true, 1) => "at least 1 element".into(),
        (true, c) => eco_format!("at least {c} elements"),
        (false, 0) => "an empty array".into(),
        (false, 1) => "a single element".into(),
        (false, c) => eco_format!("{c} elements",),
    };

    error!(
        span, "{quantifier} elements to destructure";
        hint: "the provided array has a length of {len}, \
               but the pattern expects {expected}",
    )
}
