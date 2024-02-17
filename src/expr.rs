use crate::*;
use anyhow::Result;
#[derive(Debug)]
pub enum Expr {
    BinaryOp(BinaryOp),
    Factor(Factor),
}


impl AstTrait for Expr {
    fn accept<'a>(&self, exprs: &State, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        match self {
            Expr::BinaryOp(ast) => AstTrait::accept(ast, exprs, v),
            Expr::Factor(ast) => AstTrait::accept(ast, exprs, v),
        }
    }
}
