mod access;
mod closure;
mod eval;
mod module;
pub mod opcodes;
pub mod ops;
mod pattern;
mod tracer;
mod values;

use std::sync::Arc;

use comemo::{Track, Tracked, TrackedMut};
use ecow::EcoVec;
use smallvec::SmallVec;
use typst_syntax::{Source, Span};

use crate::compiler::compile_module;
use crate::diag::{bail, SourceResult, StrResult};
use crate::engine::{Engine, Route};
use crate::foundations::{
    Content, IntoValue, Label, Module, NativeElement, Recipe, SequenceElem, Styles,
    Unlabellable, Value,
};
use crate::introspection::{Introspector, Locator};
use crate::{Library, World};

pub use self::access::*;
pub use self::closure::*;
pub use self::eval::*;
pub use self::module::*;
use self::opcodes::{Opcode, Run};
pub use self::pattern::*;
pub use self::tracer::*;
pub use self::values::*;

/// Evaluate a source file and return the resulting module.
#[comemo::memoize]
#[typst_macros::time(name = "eval", span = source.root().span())]
pub fn eval(
    world: Tracked<dyn World + '_>,
    route: Tracked<Route>,
    tracer: TrackedMut<Tracer>,
    source: &Source,
) -> SourceResult<Module> {
    // Prevent cyclic evaluation.
    let id = source.id();
    if route.contains(id) {
        panic!("Tried to cyclicly evaluate {:?}", id.vpath());
    }

    // Prepare the engine.
    let mut locator = Locator::new();
    let introspector = Introspector::default();
    let mut engine = Engine {
        world,
        introspector: introspector.track(),
        route: Route::extend(route).with_id(id),
        locator: &mut locator,
        tracer,
    };

    // Compile the module
    let compiled = compile_module(source, &mut engine)?;

    // Evaluate the module
    run_module(source, &compiled, &mut engine)
}

bitflags::bitflags! {
    /// The current state of the VM.
    struct State: u16 {
        /// The VM is currently running.
        const LOOPING = 0b0000_0001;
        /// The VM is currently joining.
        const JOINING = 0b0000_0010;
        /// The VM is currently displaying.
        const DISPLAY = 0b0000_0100;
        /// The VM is currently breaking.
        const BREAKING = 0b0001_0000;
        /// The VM is currently continuing.
        const CONTINUING = 0b0010_0000;
        /// The VM is currently returning.
        const RETURNING = 0b0100_0000;
        /// Force the VM to return the `vm.output`.
        const FORCE_RETURNING = 0b1100_0000;
        /// The VM is done.
        const DONE = 0b1_0000_0000;
    }
}

impl State {
    const fn is_looping(&self) -> bool {
        self.contains(Self::LOOPING)
    }

    const fn is_breaking(&self) -> bool {
        self.contains(Self::BREAKING)
    }

    const fn is_continuing(&self) -> bool {
        self.contains(Self::CONTINUING)
    }

    const fn is_returning(&self) -> bool {
        self.contains(Self::RETURNING)
    }

    const fn is_force_return(&self) -> bool {
        self.contains(Self::FORCE_RETURNING)
    }

    const fn is_done(&self) -> bool {
        self.contains(Self::DONE)
    }

    const fn is_running(&self) -> bool {
        !self.is_done()
    }

    const fn is_joining(&self) -> bool {
        self.contains(Self::JOINING)
    }

    const fn is_display(&self) -> bool {
        self.is_joining() && self.contains(Self::DISPLAY)
    }
}

pub fn run<I: Iterator<Item = Value>>(
    engine: &mut Engine,
    state: &mut VMState,
    instructions: &[Opcode],
    spans: &[Span],
    mut iterator: Option<I>,
) -> SourceResult<ControlFlow> {
    fn next<'a>(state: &mut VMState, instructions: &'a [Opcode]) -> Option<&'a Opcode> {
        if state.instruction_pointer == instructions.len() {
            state.state.insert(State::DONE);
            return None;
        }

        debug_assert!(state.instruction_pointer + 1 <= instructions.len());
        Some(&instructions[state.instruction_pointer])
    }

    while state.state.is_running() {
        let Some(opcode) = next(state, instructions) else {
            state.state.insert(State::DONE);
            break;
        };

        let idx = state.instruction_pointer;

        opcode.run(
            &instructions,
            &spans,
            spans[idx],
            state,
            engine,
            iterator.as_mut(),
        )?;

        if matches!(opcode, Opcode::Flow) {
            if state.state.is_looping() {
                if state.state.is_continuing() {
                    state.instruction_pointer = 0;
                    state.state.remove(State::CONTINUING);
                    continue;
                } else if state.state.is_breaking() || state.state.is_returning() {
                    // In theory, the compiler should make sure that this is valid.
                    break;
                }
            } else if state.state.is_breaking()
                || state.state.is_continuing()
                || state.state.is_returning()
            {
                // In theory, the compiler should make sure that this is valid.
                break;
            }
        }
    }

    let output = if let Some(readable) = state.output {
        match readable {
            Readable::Reg(reg) => Some(state.take(reg)),
            Readable::None => Some(Value::None),
            Readable::Bool(b) => Some(Value::Bool(b)),
            _ => Some(state.read(readable).clone()),
        }
    } else if let Some(joined) = state.joined.take() {
        Some(joined.collect(engine)?)
    } else {
        None
    };

    if state.state.is_continuing() && !state.state.is_looping() {
        Ok(ControlFlow::Continue(output.unwrap_or(Value::None)))
    } else if state.state.is_breaking() && !state.state.is_looping() {
        Ok(ControlFlow::Break(output.unwrap_or(Value::None)))
    } else if state.state.is_returning() {
        Ok(ControlFlow::Return(
            output.unwrap_or(Value::None),
            state.state.is_force_return(),
        ))
    } else {
        Ok(ControlFlow::Done(output.unwrap_or(Value::None)))
    }
}

#[derive(Debug)]
pub enum ControlFlow {
    Done(Value),
    Break(Value),
    Continue(Value),
    Return(Value, bool),
}
pub struct VMState<'a> {
    /// The current state of the VM.
    state: State,
    /// The current instruction pointer.
    instruction_pointer: usize,
    /// The joined values.
    joined: Option<Joiner>,
    /// The global library.
    global: &'a Library,
    /// The constants.
    constants: &'a [Value],
    /// The jump table.
    jumps: &'a [usize],
    /// The output register, if any.
    output: Option<Readable>,
    /// The strings.
    /// These are stored as [`Value`] but they are always [`Value::Str`].
    strings: &'a [Value],
    /// The labels.
    labels: &'a [Label],
    /// The closures.
    closures: &'a [CompiledClosure],
    /// The access patterns.
    accesses: &'a [Access],
    /// The destructure patterns.
    patterns: &'a [Pattern],
    /// The spans used in the instructions.
    spans: &'a [Span],
    /// The registers.
    registers: SmallVec<[Value; 16]>,
}

impl<'a> VMState<'a> {
    /// Read a value from the VM.
    pub fn read<T: VmRead>(&self, readable: T) -> T::Output<'_> {
        readable.read(self)
    }

    pub fn take(&mut self, register: Register) -> Value {
        std::mem::take(&mut self.registers[register.0 as usize])
    }

    /// Write a value to the VM, returning a mutable reference to the value.
    pub fn write(&mut self, writable: impl VmWrite) -> &mut Value {
        writable.write(self)
    }

    /// Write a value to the VM.
    pub fn write_one(
        &mut self,
        writable: impl VmWrite,
        value: impl IntoValue,
    ) -> StrResult<()> {
        writable.write_one(self, value)
    }

    /// Join a value to the current joining state.
    pub fn join(&mut self, value: impl IntoValue) -> StrResult<()> {
        if !self.state.is_joining() {
            bail!("cannot join in non-joining state");
        }

        // Convert the value to a display value if we are in display mode.
        let value = value.into_value();

        // We don't join `None`.
        if value.is_none() {
            return Ok(());
        }

        if let Some(joiner) = self.joined.take() {
            self.joined = Some(joiner.join(value)?);
        } else if self.state.is_display() {
            self.joined =
                Some(Joiner::Display(SequenceElem::new(vec![value.display().into()])));
        } else {
            self.joined = Some(Joiner::Value(value));
        }

        Ok(())
    }

    pub fn styled(&mut self, styles: Styles) -> StrResult<()> {
        if !self.state.is_joining() {
            bail!("cannot style in non-joining state");
        }

        if let Some(joiner) = self.joined.take() {
            self.joined = Some(joiner.styled(styles));
        } else {
            self.joined = Some(Joiner::Styled {
                parent: None,
                content: SequenceElem::new(vec![]),
                styles,
            });
        }

        Ok(())
    }

    pub fn recipe(&mut self, recipe: Recipe) -> StrResult<()> {
        if !self.state.is_joining() {
            bail!("cannot style in non-joining state");
        }

        if let Some(joiner) = self.joined.take() {
            self.joined = Some(joiner.recipe(recipe));
        } else {
            self.joined = Some(Joiner::Recipe {
                parent: None,
                content: SequenceElem::new(vec![]),
                recipe,
            });
        }

        Ok(())
    }

    /// Instantiate a closure.
    #[typst_macros::time(name = "instantiate closure", span = closure.inner.span)]
    pub fn instantiate(&self, closure: &CompiledClosure) -> SourceResult<Closure> {
        // Load the default values for the parameters.
        let mut params = EcoVec::with_capacity(closure.params.len());
        for param in &closure.params {
            match param {
                CompiledParam::Pos(target, pos) => {
                    params.push((Some(*target), Param::Pos(pos.clone())))
                }
                CompiledParam::Named { target, name, default, .. } => {
                    params.push((
                        Some(*target),
                        Param::Named {
                            name: name.clone(),
                            default: self.read(*default).cloned(),
                        },
                    ));
                }
                CompiledParam::Sink(span, target, name) => {
                    params.push((*target, Param::Sink(*span, name.clone())));
                }
            }
        }

        // Load the captured values.
        let mut captures = EcoVec::with_capacity(closure.captures.len());
        for capture in &closure.captures {
            captures.push((capture.location, self.read(capture.value).clone()));
        }

        Ok(Closure::new(
            Arc::clone(&closure.inner),
            params,
            captures,
            closure.self_storage,
        ))
    }

    /// Enter a new scope.
    pub fn enter_scope<I: Iterator<Item = Value>>(
        &mut self,
        engine: &mut Engine,
        instructions: &[Opcode],
        spans: &[Span],
        iterator: Option<I>,
        mut output: Option<Readable>,
        joins: bool,
        content: bool,
        looping: bool,
    ) -> SourceResult<ControlFlow> {
        let mut state = State::empty()
            | if looping || iterator.is_some() { State::LOOPING } else { State::empty() }
            | if joins { State::JOINING } else { State::empty() }
            | if content { State::DISPLAY } else { State::empty() };

        let mut joiner = None;
        let mut instruction_pointer = 0;

        std::mem::swap(&mut self.state, &mut state);
        std::mem::swap(&mut self.output, &mut output);
        std::mem::swap(&mut self.joined, &mut joiner);
        std::mem::swap(&mut self.instruction_pointer, &mut instruction_pointer);

        let out = run(engine, self, instructions, spans, iterator)?;

        std::mem::swap(&mut self.state, &mut state);
        std::mem::swap(&mut self.output, &mut output);
        std::mem::swap(&mut self.joined, &mut joiner);
        std::mem::swap(&mut self.instruction_pointer, &mut instruction_pointer);

        Ok(out)
    }
}

#[derive(Debug, Clone)]
enum Joiner {
    Value(Value),
    Display(SequenceElem),
    Styled { parent: Option<Box<Joiner>>, styles: Styles, content: SequenceElem },
    Recipe { parent: Option<Box<Joiner>>, recipe: Recipe, content: SequenceElem },
}

impl Joiner {
    #[typst_macros::time(name = "join")]
    pub fn join(self, other: Value) -> StrResult<Joiner> {
        if other.is_none() {
            return Ok(self);
        }

        if let Value::Label(label) = other {
            match self {
                Self::Value(value) => Ok(Joiner::Value(ops::join(value, other)?)),
                Self::Display(mut content) => {
                    let Some(last) = content
                        .children_mut()
                        .rev()
                        .find(|elem| !elem.can::<dyn Unlabellable>())
                    else {
                        bail!("nothing to label");
                    };

                    last.update(|elem| elem.set_label(label));

                    Ok(Joiner::Display(content))
                }
                Self::Styled { parent, mut content, styles } => {
                    let Some(last) = content
                        .children_mut()
                        .rev()
                        .find(|elem| !elem.can::<dyn Unlabellable>())
                    else {
                        bail!("nothing to label");
                    };

                    last.update(|elem| elem.set_label(label));

                    Ok(Joiner::Styled { parent, content, styles })
                }
                Self::Recipe { parent, recipe, mut content } => {
                    let Some(last) = content
                        .children_mut()
                        .rev()
                        .find(|elem| !elem.can::<dyn Unlabellable>())
                    else {
                        bail!("nothing to label");
                    };

                    last.update(|elem| elem.set_label(label));

                    Ok(Joiner::Recipe { parent, content, recipe })
                }
            }
        } else {
            match self {
                Self::Value(value) => Ok(Joiner::Value(ops::join(value, other)?)),
                Self::Display(mut content) => {
                    content.push(other.display());
                    Ok(Joiner::Display(content))
                }
                Self::Styled { parent, mut content, styles } => {
                    content.push(other.display());
                    Ok(Joiner::Styled { parent, content, styles })
                }
                Self::Recipe { parent, recipe, mut content } => {
                    content.push(other.display());
                    Ok(Joiner::Recipe { parent, content, recipe })
                }
            }
        }
    }

    pub fn styled(self, to_add: Styles) -> Joiner {
        if let Self::Styled { parent, content, mut styles } = self {
            if content.is_empty() {
                styles.push_all(to_add);
                return Self::Styled { parent, content, styles };
            } else {
                Self::Styled {
                    parent: Some(Box::new(Self::Styled { parent, content, styles })),
                    content: SequenceElem::new(vec![]),
                    styles: to_add,
                }
            }
        } else {
            Self::Styled {
                parent: Some(Box::new(self)),
                content: SequenceElem::new(vec![]),
                styles: to_add,
            }
        }
    }

    pub fn recipe(self, recipe: Recipe) -> Joiner {
        Self::Recipe {
            parent: Some(Box::new(self)),
            content: SequenceElem::new(vec![]),
            recipe,
        }
    }

    pub fn collect(self, engine: &mut Engine) -> SourceResult<Value> {
        fn collect_inner(
            joiner: Joiner,
            engine: &mut Engine,
            rest: Option<Content>,
        ) -> SourceResult<Value> {
            Ok(match joiner {
                Joiner::Value(value) => {
                    if let Some(rest) = rest {
                        Content::sequence([value.display(), rest]).into_value()
                    } else {
                        value
                    }
                }
                Joiner::Display(mut content) => {
                    if let Some(rest) = rest {
                        content.push(rest);
                    }

                    if content.len() == 1 {
                        content.pop().unwrap().into_value()
                    } else {
                        content.into_value()
                    }
                }
                Joiner::Styled { parent, mut content, styles } => {
                    if let Some(rest) = rest {
                        content.push(rest);
                    }

                    let rest = content.pack().styled_with_map(styles);
                    if let Some(parent) = parent {
                        collect_inner(*parent, engine, Some(rest))?
                    } else {
                        rest.into_value()
                    }
                }
                Joiner::Recipe { parent, recipe, mut content } => {
                    if let Some(rest) = rest {
                        content.push(rest);
                    }

                    let rest = content.pack().styled_with_recipe(engine, recipe)?;
                    if let Some(parent) = parent {
                        collect_inner(*parent, engine, Some(rest))?
                    } else {
                        rest.into_value()
                    }
                }
            })
        }

        collect_inner(self, engine, None)
    }
}
