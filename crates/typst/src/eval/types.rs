use typst_syntax::ast;

use super::Eval;

impl Eval for ast::TypeDefinition<'_> {
    type Output = ();

    fn eval(self, vm: &mut super::Vm) -> crate::diag::SourceResult<Self::Output> {
        todo!()
    }
}
