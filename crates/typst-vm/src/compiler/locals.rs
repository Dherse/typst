use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

use ecow::{EcoString, EcoVec};
use typst_library::foundations::Value;
use typst_library::Library;
use typst_syntax::Span;

use crate::vm::Readable;

use super::{Constants, Pointer};

#[derive(Clone, Debug)]
enum SlotInfo {
    Owned { name: EcoString, span: Span, slot: usize },
    Constant { name: EcoString, span: Span, index: usize },
    Captured { name: EcoString, span: Span, slot: usize, parent_slot: usize },
}

impl SlotInfo {
    pub fn slot(&self) -> Option<usize> {
        match self {
            Self::Owned { slot, .. } | Self::Captured { slot, .. } => Some(*slot),
            Self::Constant { .. } => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Owned { span, .. }
            | Self::Captured { span, .. }
            | Self::Constant { span, .. } => *span,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Self::Owned { name, .. }
            | Self::Captured { name, .. }
            | Self::Constant { name, .. } => name,
        }
    }

    pub fn is_captured(&self) -> bool {
        matches!(self, SlotInfo::Captured { .. })
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, SlotInfo::Constant { .. })
    }
}

struct Parent {
    capturing: bool,
    parent: Rc<RefCell<Locals>>,
}

#[derive(Default)]
pub struct Locals {
    base: Option<Library>,
    parent: Option<Parent>,
    slots: HashMap<EcoString, SlotInfo>,
    next_slot: Rc<Cell<usize>>,
    last_use: HashMap<usize, (Span, Pointer)>,
    pub(super) constants: Rc<RefCell<Constants>>,
}

#[derive(Debug)]
pub enum Local {
    Slot(usize),
    Constant(usize),
    Std(usize),
    Math(usize),
}

impl Locals {
    pub fn new(library: Option<&Library>) -> Self {
        Self {
            base: library.cloned(),
            parent: None,
            slots: HashMap::default(),
            next_slot: Rc::new(Cell::new(0)),
            last_use: HashMap::default(),
            constants: Rc::new(RefCell::new(Constants::default())),
        }
    }

    pub fn len(&self) -> usize {
        self.next_slot.get()
    }

    pub fn library(&self) -> Option<Library> {
        self.base
            .clone()
            .or_else(|| self.parent.as_ref()?.parent.borrow().library())
    }

    pub fn enter(parent: Rc<RefCell<Self>>, capturing: bool) -> Rc<RefCell<Self>> {
        // If we are capturing then we restart from zero.
        let next_slot = if capturing {
            Rc::new(Cell::new(0))
        } else {
            Rc::clone(&parent.borrow().next_slot)
        };
        let base = parent.borrow().base.clone();
        let constants = if capturing {
            Rc::new(RefCell::new(Constants::default()))
        } else {
            Rc::clone(&parent.borrow().constants)
        };

        Rc::new(RefCell::new(Self {
            base,
            parent: Some(Parent { capturing, parent }),
            slots: HashMap::new(),
            next_slot,
            last_use: HashMap::new(),
            constants,
        }))
    }

    pub fn constant(&self, index: usize) -> Option<ConstValue> {
        self.constants.borrow().get(index).cloned()
    }

    pub fn declare_const(
        &mut self,
        name: EcoString,
        span: Span,
        value: ConstValue,
    ) -> usize {
        let index = self.constants.borrow_mut().insert(value);
        self.slots
            .insert(name.clone(), SlotInfo::Constant { name, span, index });
        index
    }

    pub fn declare_anonymous_const(&mut self, value: ConstValue) -> usize {
        self.constants.borrow_mut().insert(value)
    }

    pub fn declare(&mut self, name: EcoString, span: Span) -> usize {
        let slot = self.next_slot.get();
        self.slots.insert(name.clone(), SlotInfo::Owned { name, span, slot });
        self.next_slot.set(slot + 1);
        slot
    }

    pub fn get_name(&self, slot: usize) -> Option<EcoString> {
        if let Some((key, _)) =
            self.slots.iter().find(|(_, info)| info.slot() == Some(slot))
        {
            Some(key.clone())
        } else if let Some(ref parent) = self.parent {
            parent.parent.borrow().get_name(slot)
        } else {
            None
        }
    }

    fn get_inner(
        &mut self,
        name: &EcoString,
        span: Span,
        pointer: Pointer,
        declare: bool,
        math: bool,
        commit: bool,
    ) -> Option<Local> {
        if declare {
            // If we declare, then we never ask the parent (since we just declare the value)
            return Some(Local::Slot(self.declare(name.into(), span)));
        }

        if let Some(entry) = self.slots.get(name) {
            if let SlotInfo::Constant { index, .. } = entry {
                return Some(Local::Constant(*index));
            } else {
                let slot = entry.slot().unwrap();
                if !span.is_detached() && commit {
                    self.last_use.insert(slot, (span, pointer));
                }
                return Some(Local::Slot(slot));
            }
        }

        if let Some(ref parent) = self.parent {
            let mut parent_ref = parent.parent.borrow_mut();
            let parent_slot = match parent_ref.get(
                name,
                Span::detached(),
                Pointer::detached(),
                false,
                math,
            )? {
                Local::Slot(slot) => slot,
                Local::Constant(cst) => {
                    if parent.capturing {
                        // Re-declare the constant.
                        let value = parent_ref.constants.borrow().get(cst).cloned()?;
                        let new_cst = self.constants.borrow_mut().insert(value);
                        return Some(Local::Constant(new_cst));
                    } else {
                        // Get the constant from the parent.
                        return Some(Local::Constant(cst));
                    }
                }
                other => return Some(other),
            };

            if parent.capturing {
                let slot = self.next_slot.get();

                if commit {
                    self.slots.insert(
                        name.into(),
                        SlotInfo::Captured { span, slot, name: name.into(), parent_slot },
                    );

                    self.next_slot.set(slot + 1);
                }

                return Some(Local::Slot(slot));
            } else {
                return Some(Local::Slot(parent_slot));
            }
        }

        if let Some(std) = self
            .base
            .as_ref()
            .and_then(|base| base.global.scope().get_index(name))
        {
            return Some(Local::Std(std));
        }

        if math {
            if let Some(math) =
                self.base.as_ref().and_then(|base| base.math.scope().get_index(name))
            {
                return Some(Local::Math(math));
            }
        }

        None
    }

    pub fn get(
        &mut self,
        name: &EcoString,
        span: Span,
        pointer: Pointer,
        declare: bool,
        math: bool,
    ) -> Option<Local> {
        self.get_inner(name, span, pointer, declare, math, true)
    }

    pub fn last_use(&self, slot: usize) -> Option<&(Span, Pointer)> {
        self.last_use.get(&slot)
    }

    pub fn declaration_span(&self, name: &str) -> Option<Span> {
        self.slots.get(name).map(SlotInfo::span)
    }

    pub fn captured_variables(
        &self,
    ) -> impl Iterator<Item = (&EcoString, usize, usize, Span)> {
        self.slots.values().filter_map(|info| {
            let SlotInfo::Captured { name, slot, span, parent_slot } = info else {
                return None;
            };

            Some((name, *slot, *parent_slot, *span))
        })
    }

    pub fn variables(&self) -> impl Iterator<Item = (EcoString, Readable, Span)> + '_ {
        self.slots.values().filter_map(|info| match info {
            SlotInfo::Owned { name, slot, span } => {
                Some((name.clone(), Readable::Slot(*slot), *span))
            }
            SlotInfo::Constant { name, index, span } => {
                Some((name.clone(), Readable::Const(*index), *span))
            }
            _ => None,
        })
    }

    /// Check if an std variable is shadowed.
    pub fn check_std_shadowed(&mut self, var: &EcoString) -> bool {
        self.base
            .as_ref()
            .is_some_and(|base| base.global.scope().get(var).is_some())
            && self
                .get_inner(
                    var,
                    Span::detached(),
                    Pointer::detached(),
                    false,
                    false,
                    false,
                )
                .and_then(|local| match local {
                    Local::Slot(_) => Some(()),
                    _ => None,
                })
                .is_some()
    }
}

#[derive(Clone, PartialEq, Hash)]
pub enum ConstValue {
    Inline(Value),
    Deferred(usize, DeferredEntry),
}

#[derive(Clone, PartialEq, Hash)]
pub enum DeferredEntry {
    Module,
    Content(Span),
    Nested(Span, EcoVec<(Span, EcoString)>),
}

impl From<Value> for ConstValue {
    fn from(value: Value) -> Self {
        Self::Inline(value)
    }
}

impl From<(usize, DeferredEntry)> for ConstValue {
    fn from((id, loc): (usize, DeferredEntry)) -> Self {
        Self::Deferred(id, loc)
    }
}
