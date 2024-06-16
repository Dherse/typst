use comemo::Tracked;
use smallvec::SmallVec;

use crate::diag::{SourceResult, StrResult};
use crate::engine::Engine;
use crate::foundations::{
    Content, Context, IntoValue, Recipe, SequenceElem, Styles, Unlabellable, Value,
};
use crate::lang::ops;

/// A value joiner.
///
/// This behaves like a state machine that can be used to join values together.
#[derive(Default, Debug, Clone)]
pub struct Joiner {
    segments: SmallVec<[Segment; 5]>,
    is_display: bool,
}

#[derive(Debug, Clone)]
enum Segment {
    Value(Value),
    Sequence(SequenceElem),
    Style(Styles),
    Recipe(Box<Recipe>),
}

impl Joiner {
    pub fn set_display(&mut self, display: bool) {
        self.is_display = display;
    }

    pub fn with_display(mut self, display: bool) -> Self {
        self.is_display = display;
        self
    }

    pub fn join(&mut self, other: Value) -> StrResult<()> {
        if other.is_none() {
            return Ok(());
        }

        if let Value::Label(label) = other {
            let Some(last) = self.segments.last_mut() else {
                if !self.is_display {
                    self.segments.push(Segment::Value(Value::Label(label)));
                }

                return Ok(());
            };

            match last {
                Segment::Value(value) => {
                    let taken = std::mem::take(value);
                    *value = ops::join(taken, Value::Label(label))?;
                }
                Segment::Sequence(elems) => {
                    // Find the last element that is labellable.
                    let Some(last) = elems
                        .children_mut()
                        .rev()
                        .find(|elem| !elem.can::<dyn Unlabellable>())
                    else {
                        return Ok(());
                    };

                    last.set_label(label);
                }
                _ => (),
            }
        } else {
            let Some(last) = self.segments.last_mut() else {
                if self.is_display {
                    match other {
                        // Inline other sequences.
                        Value::Content(content) if content.is::<SequenceElem>() => {
                            let sequence = content.unpack::<SequenceElem>().unwrap();
                            self.segments.push(Segment::Sequence(sequence));
                        }
                        other => {
                            self.segments.push(Segment::Sequence(SequenceElem::new(
                                vec![other.display()],
                            )));
                        }
                    }
                } else {
                    self.segments.push(Segment::Value(other));
                }

                return Ok(());
            };

            match last {
                Segment::Value(value) => {
                    let taken = std::mem::take(value);
                    *value = ops::join(taken, other)?;
                }
                Segment::Sequence(elems) => {
                    match other {
                        // Inline other sequences.
                        Value::Content(content) if content.is::<SequenceElem>() => {
                            let sequence = content.unpack::<SequenceElem>().unwrap();
                            elems.children.extend(sequence.children);
                        }
                        other => elems.push(other.display()),
                    }
                }
                _ => {
                    match other {
                        // Inline other sequences.
                        Value::Content(content) if content.is::<SequenceElem>() => {
                            let sequence = content.unpack::<SequenceElem>().unwrap();
                            self.segments.push(Segment::Sequence(sequence));
                        }
                        other => self.segments.push(Segment::Sequence(
                            SequenceElem::new(vec![other.display()]),
                        )),
                    }
                }
            }
        }

        Ok(())
    }

    pub fn styled(&mut self, styles: Styles) {
        if styles.is_empty() {
            return;
        }

        let Some(Segment::Style(last)) = self.segments.last_mut() else {
            self.segments.push(Segment::Style(styles));
            return;
        };

        last.apply_iter(styles);
    }

    pub fn recipe(&mut self, recipe: Recipe) {
        self.segments.push(Segment::Recipe(Box::new(recipe)));
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn collect(
        mut self,
        engine: &mut Engine,
        context: Tracked<Context>,
    ) -> SourceResult<Value> {
        let mut current: Option<Value> = None;
        while let Some(segment) = self.segments.pop() {
            match segment {
                Segment::Value(value) => {
                    if let Some(taken) = current.take() {
                        // We know that if this is the case, then we are in a display context.
                        // This is because we can never not have joined a value at this point.
                        debug_assert!(self.is_display || taken.is_content());
                        current = Some(
                            SequenceElem::new(vec![value.display(), taken.display()])
                                .into_value(),
                        );
                    } else {
                        current = Some(value);
                    }
                }
                Segment::Sequence(mut sequence) => {
                    if let Some(taken) = current.take() {
                        sequence.push(taken.display());
                    }

                    // Special handling to inline single element sequences.
                    if sequence.len() == 1 {
                        current = Some(sequence.pop().unwrap().into_value());
                    } else {
                        current = Some(sequence.into_value());
                    }
                }
                Segment::Style(style) => {
                    if let Some(taken) = current.take() {
                        current =
                            Some(taken.display().styled_with_map(style).into_value());
                    } else {
                        current =
                            Some(Content::empty().styled_with_map(style).into_value());
                    }
                }
                Segment::Recipe(recipe) => {
                    if let Some(taken) = current.take() {
                        current = Some(
                            taken
                                .display()
                                .styled_with_recipe(engine, context, *recipe)?
                                .into_value(),
                        );
                    } else {
                        current = Some(
                            Content::empty()
                                .styled_with_recipe(engine, context, *recipe)?
                                .into_value(),
                        );
                    }
                }
            }
        }

        Ok(current.unwrap_or(Value::None))
    }
}
