mod access;
mod binary;
mod call;
mod closure;
mod destruct;
mod flow;
pub mod instructions;
mod joiner;
mod markup;
mod math;
mod ops;
mod rules;
mod state;
mod unary;

pub use self::access::{AccessSegment, MutAccess};
pub use self::call::ArgSegment;
pub use self::destruct::{Pattern, PatternItem};
pub use self::flow::Iterable;
pub(crate) use self::instructions::{FlowOp, Instruction, Instructions, Noop};
use self::joiner::Joiner;
pub use self::state::ControlFlow;
use self::state::{Flow, State};

use std::borrow::Cow;
use std::mem;

use comemo::Tracked;
use typst_library::diag::{bail, error, At, HintedStrResult, HintedString, SourceResult};
use typst_library::engine::Engine;
use typst_library::foundations::{Content, Context, IntoValue, Recipe, Styles, Value};
use typst_library::{Library, World};
use typst_syntax::Span;
use typst_utils::Scalar;

use crate::compiler::{Local, Pointer, PointerRange};

/// The maximum number of loop iterations.
const MAX_ITERATIONS: usize = 10_000;

pub(crate) struct Vm<'a> {
    base: &'a Library,
    instructions: &'a [Instructions],
    index: usize,
    iterations: usize,

    constants: &'a [Value],
    stack: Vec<Value>,
    slots: &'a mut Vec<Value>,

    #[cfg(debug_assertions)]
    lock: bool,

    state: State,

    engine: Engine<'a>,
    context: Tracked<'a, Context<'a>>,

    joiner: Joiner,

    output: Option<(Span, Readable)>,
}

impl<'a> Vm<'a> {
    pub fn new(
        base: &'a Library,
        instructions: &'a [Instructions],
        constants: &'a [Value],
        slots: &'a mut Vec<Value>,
        engine: Engine<'a>,
        context: Tracked<'a, Context<'a>>,
    ) -> Self {
        Self {
            base,
            instructions,
            index: 0,
            iterations: 0,
            constants,
            stack: Vec::with_capacity(16),
            slots,
            #[cfg(debug_assertions)]
            lock: false,
            state: State::empty(),
            engine,
            context,
            joiner: Joiner::default(),
            output: None,
        }
    }

    /// Access the underlying world.
    pub fn world(&self) -> Tracked<'a, dyn World + 'a> {
        self.engine.world
    }

    pub fn with_display(mut self, display: bool) -> Self {
        self.joiner = self.joiner.with_display(display);
        self
    }
}

impl Vm<'_> {
    pub fn run(&mut self, iterator: Option<&mut Iterable>) -> SourceResult<ControlFlow> {
        self.run_until(iterator, Pointer::detached())
    }

    pub fn run_until(
        &mut self,
        mut iterator: Option<&mut Iterable>,
        end: Pointer,
    ) -> SourceResult<ControlFlow> {
        fn next<'a, 'b>(vm: &'b mut Vm<'a>, end: usize) -> Option<&'a Instructions> {
            if vm.index >= end || vm.index >= vm.instructions.len() {
                vm.state.done();
                return None;
            }

            debug_assert!(vm.index < vm.instructions.len());
            Some(&vm.instructions[vm.index])
        }

        let start = self.index;
        while self.state.is_running() {
            let Some(isr) = next(self, end.as_raw()) else {
                self.state.done();
                break;
            };

            self.index += 1;
            isr.eval(self, iterator.as_mut().map(|r| &mut **r))?;

            if isr.is_flow() && !matches!(self.state.flow, Flow::None) {
                if self.state.is_looping() {
                    match self.state.flow {
                        Flow::None => {}
                        Flow::Continue => {
                            self.index = start;
                            self.state.flow = Flow::None;
                            continue;
                        }
                        Flow::Break | Flow::Return(_) | Flow::Done => {
                            self.state.set_done();
                            break;
                        }
                    }
                } else {
                    match self.state.flow {
                        Flow::None => {}
                        Flow::Continue | Flow::Break | Flow::Return(_) | Flow::Done => {
                            break;
                        }
                    }
                }
            }
        }

        let output = if let Some((span, readable)) = self.output {
            Some(self.get(readable, span)?.into_owned())
        } else if !self.joiner.is_empty() {
            Some(mem::take(&mut self.joiner).collect(&mut self.engine, self.context)?)
        } else if self.state.is_display() && !self.state.is_looping() {
            Some(Content::empty().into_value())
        } else {
            None
        };

        Ok(match self.state.flow {
            Flow::Break => ControlFlow::Break(output.unwrap_or(Value::None)),
            Flow::Continue => ControlFlow::Continue(output.unwrap_or(Value::None)),
            Flow::Return(forced) => {
                ControlFlow::Return(output.unwrap_or(Value::None), forced)
            }
            _ => ControlFlow::Done(output.unwrap_or(Value::None)),
        })
    }

    pub fn jump(&mut self, pointer: Pointer, inc: bool) -> HintedStrResult<()> {
        if pointer.as_raw() > self.instructions.len() {
            bail!(
                "tried jumping to an out-of-bound instruction: {} > {}",
                pointer.as_raw(),
                self.instructions.len();
                hint: "this is a compiler bug"
            );
        }

        if inc {
            self.inc()?;
        }

        self.index = pointer.as_raw();

        Ok(())
    }

    pub fn inc(&mut self) -> HintedStrResult<()> {
        self.iterations += 1;
        if self.iterations > MAX_ITERATIONS {
            bail!("loop seems to be infinite");
        }

        Ok(())
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn top(&self) -> HintedStrResult<&Value> {
        self.stack.last().ok_or_else(pop_empty_stack)
    }

    pub fn top_mut(&mut self) -> HintedStrResult<&mut Value> {
        self.stack.last_mut().ok_or_else(pop_empty_stack)
    }

    pub fn pop(&mut self) -> HintedStrResult<Value> {
        self.stack.pop().ok_or_else(pop_empty_stack)
    }

    pub fn get(&mut self, value: Readable, span: Span) -> SourceResult<Cow<Value>> {
        Ok(match value {
            Readable::None | Readable::Empty => Cow::Owned(Value::None),
            Readable::Stack => Cow::Owned(self.pop().at(span)?),
            Readable::Slot(slot) => {
                #[cfg(debug_assertions)]
                assert!(!self.lock, "attempting to read from a locked VM");

                let Some(value) = self.slots.get(slot) else {
                    return Err(slot_out_of_bounds(slot)).at(span);
                };

                Cow::Borrowed(value)
            }
            Readable::Take(slot) => {
                #[cfg(debug_assertions)]
                assert!(!self.lock, "attempting to read from a locked VM");

                let Some(value) = self.slots.get_mut(slot) else {
                    return Err(slot_out_of_bounds(slot)).at(span);
                };

                Cow::Owned(mem::take(value))
            }
            Readable::Const(const_) => {
                let Some(value) = self.constants.get(const_) else {
                    return Err(const_out_of_bounds(const_)).at(span);
                };

                Cow::Borrowed(value)
            }
            Readable::Bool(val) => Cow::Owned(Value::Bool(val)),
            Readable::Int(val) => Cow::Owned(Value::Int(val)),
            Readable::Float(val) => Cow::Owned(Value::Float(val.get())),
            Readable::Auto => Cow::Owned(Value::Auto),
            Readable::Std(idx) => self
                .base
                .global
                .field_by_index(idx, (&mut self.engine, span))
                .map(Cow::Borrowed)
                .at(span)?,
            Readable::Math(idx) => self
                .base
                .math
                .field_by_index(idx, (&mut self.engine, span))
                .map(Cow::Borrowed)
                .at(span)?
                .clone(),
        })
    }

    pub fn access<O>(
        &mut self,
        access: &MutAccess,
        method: impl FnOnce(&mut Vm<'_>, &mut Value) -> SourceResult<O>,
    ) -> SourceResult<O> {
        access.eval(self, method)
    }

    pub fn set(&mut self, access: &MutAccess, to: Value) -> SourceResult<()> {
        access.set(self, to)
    }

    /// Applies a styling to the current joining state.
    pub fn styled(&mut self, styles: Styles) {
        self.joiner.styled(styles);
    }

    /// Applies a recipe to the current joining state.
    pub fn recipe(&mut self, recipe: Recipe) {
        self.joiner.recipe(recipe);
    }

    /// Locks the VM to get mutable access to one of its slots.
    ///
    /// During the lock, no other access whether mutable or immutable can be done
    /// to any other slots.
    pub fn lock<O>(
        &mut self,
        slot: usize,
        span: Span,
        method: impl FnOnce(&mut Self, &mut Value) -> SourceResult<O>,
    ) -> SourceResult<O> {
        #[cfg(debug_assertions)]
        {
            assert!(!self.lock, "attempting to lock a locked VM");
            self.lock = true;
        }

        // Take the slot: replaces by default value which does
        // not allocate, equivalent memcpy of 24 bytes.
        let mut slots = mem::take(self.slots);
        let Some(slot) = slots.get_mut(slot) else {
            bail!(
                span,
                "slot out of bounds: the len is {} but the index is {slot}", slots.len();
                hint: "this is a compiler bug"
            );
        };

        let out = method(self, slot)?;

        // Replace the slot, drops the old unallocated value
        *self.slots = slots;

        #[cfg(debug_assertions)]
        {
            self.lock = false;
        }

        Ok(out)
    }

    pub fn enter_scope<'b>(
        &'b mut self,
        range: PointerRange,
        iterator: Option<&mut Iterable>,
        mut output: Option<(Span, Readable)>,
        content: bool,
        looping: bool,
    ) -> SourceResult<ControlFlow> {
        // The state is built for the current scope, this means that we need to swap the state
        // with the current state.
        // Regarding looping, we do not care if a higher scope is looping, we only care if the
        // current scope is looping for control flow purposes.
        let mut state = State::empty().loop_(looping).display(content);
        let mut joiner = Joiner::default().with_display(content);
        let mut iterations = 0;

        mem::swap(&mut self.state, &mut state);
        mem::swap(&mut self.output, &mut output);
        mem::swap(&mut self.joiner, &mut joiner);
        mem::swap(&mut self.iterations, &mut iterations);

        self.index = range.start().as_raw();
        let out = self.run_until(iterator, range.end())?;

        mem::swap(&mut self.state, &mut state);
        mem::swap(&mut self.output, &mut output);
        mem::swap(&mut self.joiner, &mut joiner);
        mem::swap(&mut self.iterations, &mut iterations);

        self.index = range.end().as_raw();

        Ok(out)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum Readable {
    Empty,
    None,
    Stack,
    Auto,
    Slot(usize),
    Take(usize),
    Const(usize),
    Bool(bool),
    Int(i64),
    Float(Scalar),
    Std(usize),
    Math(usize),
}

impl Readable {
    fn take_slot(&mut self, slot: usize) {
        match self {
            Self::Slot(s) if *s == slot => *self = Self::Take(slot),
            _ => {}
        }
    }

    pub fn is_dyn(&self) -> bool {
        matches!(self, Self::Stack | Self::Slot(_) | Self::Take(_))
    }
}

impl From<Local> for Readable {
    fn from(value: Local) -> Self {
        match value {
            Local::Slot(slot) => Self::Slot(slot),
            Local::Std(std) => Self::Std(std),
            Local::Math(math) => Self::Math(math),
            Local::Constant(cst) => Self::Const(cst),
        }
    }
}

#[cold]
fn slot_out_of_bounds(slot: usize) -> HintedString {
    error!(
        "slot {slot} out of bounds";
        hint: "this is a compiler bug"
    )
}

#[cold]
fn const_out_of_bounds(const_: usize) -> HintedString {
    error!(
        "constant {const_} out of bounds";
        hint: "this is a compiler bug"
    )
}

#[cold]
fn pop_empty_stack() -> HintedString {
    error!(
        "trying to pop from an empty stack";
        hint: "this is a compiler bug"
    )
}
