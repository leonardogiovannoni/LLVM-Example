use crate::*;
use anyhow::Result;
use enum_dispatch::enum_dispatch;
#[derive(Debug, Clone)]
#[enum_dispatch]
pub enum Expr {
    BinaryOp(BinaryOp),
    Factor(Factor),
}

impl<'a> Default for Expr {
    fn default() -> Self {
        Expr::BinaryOp(BinaryOp::default())
    }
}

impl AstTrait for Expr {
    fn accept<'a>(&self, exprs: &State, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        match self {
            Expr::BinaryOp(ast) => AstTrait::accept(ast, exprs, v),
            Expr::Factor(ast) => AstTrait::accept(ast, exprs, v),
        }
    }

    fn callbacks_mut(
        &mut self,
        bin_op: Option<impl FnOnce(&mut BinaryOp) -> Result<()>>,
        factor: Option<impl FnOnce(&mut Factor) -> Result<()>>,
        with_decl: Option<impl FnOnce(&mut WithDecl) -> Result<()>>,
        index: Option<impl FnOnce(&mut ExprIndex) -> Result<()>>,
    ) -> Result<()> {
        match self {
            Expr::BinaryOp(b) => AstTrait::callbacks_mut(b, bin_op, factor, with_decl, index),
            Expr::Factor(f) => AstTrait::callbacks_mut(f, bin_op, factor, with_decl, index),
        }
    }

    fn callbacks(
        &self,
        bin_op: Option<impl FnOnce(&BinaryOp) -> Result<()>>,
        factor: Option<impl FnOnce(&Factor) -> Result<()>>,
        with_decl: Option<impl FnOnce(&WithDecl) -> Result<()>>,
        index: Option<impl FnOnce(&ExprIndex) -> Result<()>>,
    ) -> Result<()> {
        match self {
            Expr::BinaryOp(b) => AstTrait::callbacks(b, bin_op, factor, with_decl, index),
            Expr::Factor(f) => AstTrait::callbacks(f, bin_op, factor, with_decl, index),
        }
    }
}
