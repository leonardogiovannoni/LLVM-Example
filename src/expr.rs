use crate::*;
use enum_dispatch::enum_dispatch;

#[derive(Debug, Clone)]
#[enum_dispatch]
pub enum Expr<'a> {
    BinaryOp(BinaryOp),
    Factor(Factor<'a>),
}

impl<'a> Default for Expr<'a> {
    fn default() -> Self {
        Expr::BinaryOp(BinaryOp::default())
    }
}

#[enum_dispatch(Expr)]
trait ExprTrait<'a> {
    fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn ASTVisitorTrait<'a>);
}

impl<'a> ExprTrait<'a> for BinaryOp {
    fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn ASTVisitorTrait<'a>) {
        ASTTrait::accept(self, exprs, v)
    }
}

impl<'a> ExprTrait<'a> for Factor<'a> {
    fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn ASTVisitorTrait<'a>) {
        ASTTrait::accept(self, exprs, v)
    }
}

impl<'a> ExprTrait<'a> for ExprIndex {
    fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn ASTVisitorTrait<'a>) {
        ASTTrait::accept(self, exprs, v)
    }
}
