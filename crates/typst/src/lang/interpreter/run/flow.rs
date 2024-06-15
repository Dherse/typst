use typst_syntax::Span;
use unicode_segmentation::UnicodeSegmentation;

use crate::diag::{bail, At, SourceResult};
use crate::engine::{Engine, Route};
use crate::foundations::{Func, Value};
use crate::lang::compiler::{import_value, ImportedModule};
use crate::lang::interpreter::{ControlFlow, Vm};
use crate::lang::opcodes::{
    Break, Call, Continue, Field, Include, Instantiate, InstantiateModule, Iter, Next,
    Opcode, Return, ReturnVal, While,
};
use crate::lang::operands::Readable;
use crate::lang::operands::Register;

use super::{Iterable, Run, SimpleRun};

impl SimpleRun for InstantiateModule {
    fn run(&self, span: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        // Load the path to the module.
        let path = vm.read(self.path);

        // Load the module description
        let module = vm.read(self.module);

        // Load the module, we know it's static.
        let ImportedModule::Static(loaded) = import_value(engine, path, span, true)?
        else {
            bail!(span, "expected static module, found dynamic module");
        };

        // Iterate over the module description and apply the rules.
        for value in &module.imports {
            let mut names = value.names.iter();
            let first = loaded.field(names.next().unwrap()).at(value.span)?;
            let field = names.try_fold(first.clone(), |field, name| {
                field.field(name).at(value.span)
            })?;

            // Apply the rule.
            vm.write_one(value.location, field).at(value.span)?;
        }

        // Write the module to the output.
        vm.write_one(self.out, Value::Module(loaded)).at(span)?;

        Ok(())
    }
}

impl SimpleRun for Include {
    fn run(&self, span: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        // Load the path to the module.
        let path = vm.read(self.path);

        // Load the module, we know it's static.
        let ImportedModule::Static(loaded) = import_value(engine, path, span, false)?
        else {
            bail!(span, "expected static module, found dynamic module");
        };

        // Write the module's content to the output.
        vm.write_one(self.out, loaded.content()).at(span)?;

        Ok(())
    }
}

impl SimpleRun for Instantiate {
    fn run(&self, span: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        // Get the closure.
        let closure = vm.read(self.closure);
        let closure_span = closure.span();

        // Instantiate the closure. This involves:
        // - Capturing all necessary values.
        // - Capturing the default values of named arguments.
        let closure = vm.instantiate(closure)?;

        // Write the closure to the output.
        vm.write_one(self.out, Func::from(closure.into_owned()).spanned(closure_span))
            .at(span)?;

        Ok(())
    }
}

impl SimpleRun for Call {
    fn run(&self, span: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        // Check that we're not exceeding the call depth limit.
        if !engine.route.within(Route::MAX_CALL_DEPTH) {
            bail!(span, "maximum function call depth exceeded");
        }

        // Get the function.
        let accessor = vm.read(self.access);

        // Run the closure using an access.
        let value = accessor.get_value(vm, engine)?;

        // Write the value to the output.
        vm.write_one(self.out, value).at(span)?;

        Ok(())
    }
}

impl SimpleRun for Field {
    fn run(&self, span: Span, vm: &mut Vm, engine: &mut Engine) -> SourceResult<()> {
        // Get the value.
        let value = vm.read(self.access).get_value(vm, engine)?;

        // Write the value to the output.
        // TODO: improve efficiency by removing cloning!
        vm.write_one(self.out, value).at(span)?;

        Ok(())
    }
}

impl Run for While {
    #[typst_macros::time(name = "while loop", span = span)]
    fn run(
        &self,
        instructions: &[Opcode],
        spans: &[Span],
        span: Span,
        vm: &mut Vm,
        engine: &mut Engine,
        _: Option<&mut Iterable>,
    ) -> SourceResult<()> {
        let instructions = &instructions[..self.len as usize];
        let spans = &spans[..self.len as usize];

        // Runt the loop inside a new scope.
        let flow =
            vm.enter_scope(engine, instructions, spans, None, None, self.content, true)?;

        let mut forced_return = false;
        let output = match flow {
            ControlFlow::Done(value) => value,
            ControlFlow::Break(_) | ControlFlow::Continue(_) => {
                bail!(span, "unexpected control flow, malformed instruction")
            }
            ControlFlow::Return(value, forced) => {
                vm.state.set_returning(forced);
                forced_return = forced;
                value
            }
        };

        if forced_return {
            let reg = Register(0);
            vm.write_one(reg, output).at(span)?;
            vm.output = Some(Readable::reg(reg));
        } else {
            // Write the output to the output register.
            vm.write_one(self.out, output).at(span)?;
        }

        vm.bump(self.len as usize);

        Ok(())
    }
}

impl Run for Iter {
    #[typst_macros::time(name = "for loop", span = span)]
    fn run(
        &self,
        instructions: &[Opcode],
        spans: &[Span],
        span: Span,
        vm: &mut Vm,
        engine: &mut Engine,
        _: Option<&mut Iterable>,
    ) -> SourceResult<()> {
        // Get the iterable.
        let iterable = vm.read(self.iterable).clone();
        let instructions = &instructions[..self.len as usize];

        macro_rules! iter {
            (for $iterable:expr) => {{
                let mut iter = Iterable::from($iterable.into_iter());
                vm.enter_scope(
                    engine,
                    instructions,
                    spans,
                    Some(&mut iter),
                    None,
                    self.content,
                    true,
                )?
            }};
        }

        let iterable_type = iterable.ty();
        let flow = match iterable {
            Value::Array(array) => {
                // Iterate over values of array.
                iter!(for array.iter())
            }
            Value::Dict(dict) => {
                // Iterate over key-value pairs of dict.
                iter!(for dict.iter())
            }
            Value::Str(str) => {
                // Iterate over graphemes of string.
                iter!(for str.as_str().graphemes(true))
            }
            Value::Bytes(bytes) => {
                // Iterate over the integers of bytes.
                iter!(for bytes.as_slice().iter())
            }
            _ => {
                bail!(span, "cannot loop over {}", iterable_type);
            }
        };

        let mut forced_return = false;
        let output = match flow {
            ControlFlow::Done(value) => value,
            ControlFlow::Break(_) | ControlFlow::Continue(_) => {
                unreachable!("unexpected control flow")
            }
            ControlFlow::Return(value, forced) => {
                vm.state.set_returning(forced);
                forced_return = forced;
                value
            }
        };

        if forced_return {
            let reg = Register(0);
            vm.write_one(reg, output).at(span)?;
            vm.output = Some(Readable::reg(reg));
        } else {
            // Write the output to the output register.
            vm.write_one(self.out, output).at(span)?;
        }

        vm.bump(self.len as usize);

        Ok(())
    }
}

impl Run for Next {
    fn run(
        &self,
        _: &[Opcode],
        _: &[Span],
        span: Span,
        vm: &mut Vm,
        engine: &mut Engine,
        iterator: Option<&mut Iterable>,
    ) -> SourceResult<()> {
        let Some(iter) = iterator else {
            bail!(span, "not in an iterable scope");
        };

        // Get the next value.
        let Some(value) = iter.next() else {
            vm.state.set_done();
            return Ok(());
        };

        // Get the pattern.
        let pattern = vm.read(self.pattern);

        // Destructure the value.
        pattern.write_itered(vm, engine, value)?;

        Ok(())
    }
}

impl SimpleRun for Continue {
    fn run(&self, _: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        if !vm.state.is_breaking() && !vm.state.is_returning() {
            vm.state.set_continuing();
        }

        Ok(())
    }
}

impl SimpleRun for Break {
    fn run(&self, _: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        if !vm.state.is_continuing() && !vm.state.is_returning() {
            vm.state.set_breaking();
        }

        Ok(())
    }
}

impl SimpleRun for Return {
    fn run(&self, _: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        if !vm.state.is_breaking() && !vm.state.is_continuing() {
            vm.state.set_returning(vm.output.is_some());
        }

        Ok(())
    }
}

impl SimpleRun for ReturnVal {
    fn run(&self, _: Span, vm: &mut Vm, _: &mut Engine) -> SourceResult<()> {
        vm.output = Some(self.value);
        if !vm.state.is_breaking() && !vm.state.is_continuing() {
            vm.state.set_returning(vm.output.is_some());
        }

        Ok(())
    }
}
