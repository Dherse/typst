use typst_syntax::ast::{self, AstNode};
use typst_syntax::Span;

use super::call::ArgsCompile;
use super::{Compile, Compiler, IntoCompiledValue, ReadableGuard, RegisterGuard};
use crate::diag::{bail, error, At, HintedStrResult, HintedString, SourceResult};
use crate::engine::Engine;
use crate::foundations::{
    cannot_mutate_constant, unknown_variable, Func, IntoValue, Module, Type, Value,
};
use crate::lang::compiled::{CompiledAccess, CompiledAccessRoot, CompiledAccessSegment};
use crate::lang::operands::{AccessId, Global};
use crate::utils::PicoStr;

#[derive(Clone, Hash, PartialEq)]
pub struct Access {
    pub root: AccessRoot,
    pub tail: Vec<AccessTail>,
    pub mutable: bool,
    pub in_math: bool,
}

#[derive(Clone, Hash, PartialEq)]
pub enum AccessRoot {
    Register(RegisterGuard),
    Readable(ReadableGuard),
    /// Access this value via a function call.
    Call {
        /// The span of the call.
        span: Span,
        /// The function to call.
        func: ReadableGuard,
        /// The arguments to pass to the function.
        args: ReadableGuard,
        /// Whether the call has a trailing comma.
        trailing_comma: bool,
    },
}

#[derive(Clone, Hash, PartialEq)]
pub enum AccessTail {
    /// Access this value through a field.
    Field {
        /// The span of the field.
        span: Span,
        /// The name of the field.
        name: PicoStr,
    },
    /// Access this value through an accessor method.
    Method {
        /// The span of the method.
        span: Span,
        /// The span of the function name.
        name_span: Span,
        /// The name of the method.
        name: PicoStr,
        /// The readable to access the arguments from.
        args: ReadableGuard,
        /// Whether the call has a trailing comma.
        trailing_comma: bool,
    },
}

impl Access {
    pub fn register(reg: RegisterGuard, mutable: bool) -> Self {
        Self {
            root: AccessRoot::Register(reg),
            tail: vec![],
            mutable,
            in_math: false,
        }
    }
    pub fn chained(mut self, span: Span, field: PicoStr) -> Self {
        self.tail.push(AccessTail::Field { span, name: field });
        self
    }

    pub fn as_simple(&self) -> Option<RegisterGuard> {
        if !self.tail.is_empty() {
            return None;
        }

        match &self.root {
            AccessRoot::Register(reg) => Some(reg.clone()),
            AccessRoot::Readable(ReadableGuard::Register(reg)) => Some(reg.clone()),
            _ => None,
        }
    }

    pub fn with_math(mut self, in_math: bool) -> Self {
        self.in_math = in_math;
        self
    }

    pub fn resolve(&self, compiler: &Compiler) -> HintedStrResult<Option<&Value>> {
        Ok(None)
    }
}

impl IntoCompiledValue for Access {
    type CompiledValue = CompiledAccess;

    fn into_compiled_value(self) -> Self::CompiledValue {
        let root = match self.root {
            AccessRoot::Register(reg) => CompiledAccessRoot::Register(reg.into()),
            AccessRoot::Readable(read) => CompiledAccessRoot::Readable(read.into()),
            AccessRoot::Call { span, func, args, trailing_comma } => {
                CompiledAccessRoot::Call {
                    span,
                    func: func.into(),
                    args: args.into(),
                    trailing_comma,
                }
            }
        };

        let segments = self
            .tail
            .into_iter()
            .map(|tail| match tail {
                AccessTail::Field { span, name } => {
                    CompiledAccessSegment::Field { span, name: name.resolve() }
                }
                AccessTail::Method { span, name, args, name_span, trailing_comma } => {
                    CompiledAccessSegment::Method {
                        span,
                        name_span,
                        name: name.resolve(),
                        args: args.into(),
                        trailing_comma,
                    }
                }
            })
            .collect();

        CompiledAccess {
            root,
            segments,
            mutable: self.mutable,
            in_math: self.in_math,
        }
    }
}

pub trait CompileAccess: Sized {
    fn access(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
    ) -> SourceResult<Access> {
        let mut tail = vec![];
        let root = self.access_root(compiler, engine, mutable, &mut tail)?;

        Ok(Access { root, tail, mutable, in_math: false })
    }

    /// Generate an access to the value.
    fn access_root(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        tail: &mut Vec<AccessTail>,
    ) -> SourceResult<AccessRoot>;

    fn access_tail(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        out: &mut Vec<AccessTail>,
    ) -> SourceResult<()>;
}

impl CompileAccess for ast::Expr<'_> {
    fn access_root(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        tail: &mut Vec<AccessTail>,
    ) -> SourceResult<AccessRoot> {
        match self {
            Self::Ident(v) => v.access_root(compiler, engine, mutable, tail),
            Self::Parenthesized(v) => v.access_root(compiler, engine, mutable, tail),
            Self::FieldAccess(v) => v.access_root(compiler, engine, mutable, tail),
            Self::FuncCall(v) => v.access_root(compiler, engine, mutable, tail),
            _ if mutable => {
                bail!(self.span(), "cannot mutate a temporary value");
            }
            other => {
                let register = compiler.allocate();

                // Even if we allocate an unnecessary register, it is still preferable for
                // easier implementation of accesses overall.
                other.compile(compiler, engine, register.clone().into())?;
                Ok(AccessRoot::Register(register))
            }
        }
    }

    fn access_tail(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        out: &mut Vec<AccessTail>,
    ) -> SourceResult<()> {
        match self {
            Self::Ident(v) => v.access_tail(compiler, engine, mutable, out),
            Self::Parenthesized(v) => v.access_tail(compiler, engine, mutable, out),
            Self::FieldAccess(v) => v.access_tail(compiler, engine, mutable, out),
            Self::FuncCall(v) => v.access_tail(compiler, engine, mutable, out),
            _ if mutable => {
                bail!(self.span(), "cannot mutate a temporary value");
            }
            _ => Ok(()),
        }
    }
}

impl CompileAccess for ast::Ident<'_> {
    fn access_root(
        self,
        compiler: &mut Compiler,
        _: &mut Engine,
        mutable: bool,
        _: &mut Vec<AccessTail>,
    ) -> SourceResult<AccessRoot> {
        match compiler.read(self.span(), self.get(), mutable) {
            Some(ReadableGuard::Register(reg)) => Ok(AccessRoot::Register(reg)),
            Some(ReadableGuard::Captured(cap)) => {
                if mutable && compiler.contextual {
                    bail!(self.span(), "variables from outside the context expression are read-only and cannot be modified")
                } else if mutable {
                    bail!(self.span(), "variables from outside the function are read-only and cannot be modified")
                } else {
                    Ok(AccessRoot::Register(cap))
                }
            }
            Some(ReadableGuard::GlobalModule) => {
                if mutable {
                    return Err(cannot_mutate_constant(self.get())).at(self.span());
                } else {
                    Ok(AccessRoot::Readable(ReadableGuard::GlobalModule))
                }
            }
            Some(ReadableGuard::Global(global)) => {
                if mutable {
                    return Err(cannot_mutate_constant(self.get())).at(self.span());
                } else {
                    let const_ = compiler
                        .library()
                        .global
                        .field_by_index(global.as_raw() as usize)
                        .ok_or_else(|| {
                            HintedString::new(error!(
                                "could not find global `{}` in scope",
                                self.get()
                            ))
                        })
                        .at(self.span())?
                        .clone();

                    let const_id = compiler.const_(const_);
                    Ok(AccessRoot::Readable(ReadableGuard::Constant(const_id)))
                }
            }
            None => {
                // Special case for constants.
                if mutable && compiler.library().global.field(self.get()).is_ok() {
                    return Err(cannot_mutate_constant(self.get())).at(self.span());
                }

                return Err(unknown_variable(self.get())).at(self.span());
            }
            _ => bail!(self.span(), "unexpected variable access"),
        }
    }

    fn access_tail(
        self,
        _: &mut Compiler,
        _: &mut Engine,
        _: bool,
        _: &mut Vec<AccessTail>,
    ) -> SourceResult<()> {
        bail!(
            self.span(),
            "unexpected identifier, expected field access or function call"
        )
    }
}

impl CompileAccess for ast::Parenthesized<'_> {
    fn access_root(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        tail: &mut Vec<AccessTail>,
    ) -> SourceResult<AccessRoot> {
        self.expr().access_root(compiler, engine, mutable, tail)
    }

    fn access_tail(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        tail: &mut Vec<AccessTail>,
    ) -> SourceResult<()> {
        self.expr().access_tail(compiler, engine, mutable, tail)
    }
}

impl CompileAccess for ast::FieldAccess<'_> {
    fn access_root(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        tail: &mut Vec<AccessTail>,
    ) -> SourceResult<AccessRoot> {
        let root = self.target().access_root(compiler, engine, mutable, tail)?;
        let field = self.field().as_str();

        tail.push(AccessTail::Field {
            span: self.field().span(),
            name: PicoStr::new(field),
        });

        Ok(root)
    }

    fn access_tail(
        self,
        _: &mut Compiler,
        _: &mut Engine,
        _: bool,
        _: &mut Vec<AccessTail>,
    ) -> SourceResult<()> {
        bail!(self.span(), "unexpected field access, expected function call");
    }
}

impl CompileAccess for ast::FuncCall<'_> {
    fn access_root(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        tail: &mut Vec<AccessTail>,
    ) -> SourceResult<AccessRoot> {
        // Compile the arguments.
        let args = self.args();
        let args_reg = args.compile_args(compiler, engine, self.span())?;

        if let ast::Expr::FieldAccess(access) = self.callee() {
            let left = access.target().access_root(compiler, engine, mutable, tail)?;

            tail.push(AccessTail::Method {
                span: self.span(),
                name_span: access.field().span(),
                name: PicoStr::new(access.field().as_str()),
                args: args_reg,
                trailing_comma: args.trailing_comma(),
            });

            Ok(left)
        } else if !mutable {
            // Compile the function call.
            let func = self.callee().compile_to_readable(compiler, engine)?;

            Ok(AccessRoot::Call {
                span: self.span(),
                func,
                args: args_reg,
                trailing_comma: args.trailing_comma(),
            })
        } else {
            bail!(self.span(), "cannot mutate a temporary value")
        }
    }

    fn access_tail(
        self,
        compiler: &mut Compiler,
        engine: &mut Engine,
        mutable: bool,
        tail: &mut Vec<AccessTail>,
    ) -> SourceResult<()> {
        if let ast::Expr::FieldAccess(access) = self.callee() {
            // Compile the arguments.
            let args = self.args();
            let args_reg = args.compile_args(compiler, engine, self.span())?;

            access.target().access_tail(compiler, engine, mutable, tail)?;
            tail.push(AccessTail::Method {
                span: access.field().span(),
                name_span: access.field().span(),
                name: PicoStr::new(access.field().as_str()),
                args: args_reg,
                trailing_comma: args.trailing_comma(),
            });

            Ok(())
        } else {
            bail!(self.span(), "cannot mutate a temporary value")
        }
    }
}
