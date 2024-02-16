use crate::*;
use anyhow::Result;
#[derive(Debug, Clone)]
pub enum Expr {
    BinaryOp(BinaryOp),
    Factor(Factor),
}

impl Default for Expr {
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
