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
}
