use crate::*;
use anyhow::Result;
#[derive(Debug)]
pub enum Expr {
    BinaryOp(BinaryOp),
    Factor(Factor),
}

impl AstTrait for Expr {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        match self {
            Expr::BinaryOp(ast) => AstTrait::accept(ast, v),
            Expr::Factor(ast) => AstTrait::accept(ast, v),
        }
    }
}
