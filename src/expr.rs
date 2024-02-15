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
    fn accept<'a>(&mut self, exprs: &mut Vec<Expr>, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        match self {
            Expr::BinaryOp(ast) => AstTrait::accept(ast, exprs, v),
            Expr::Factor(ast) => AstTrait::accept(ast, exprs, v),
        }
    }

    fn swap(&mut self, ast: &mut Ast) {
        match self {
            Expr::BinaryOp(b) => {
                AstTrait::swap(b, ast);
            }
            Expr::Factor(f) => {
                AstTrait::swap(f, ast);
            }
        }
    }

    fn take(&mut self) -> Ast {
        match self {
            Expr::BinaryOp(b) => AstTrait::take(b),
            Expr::Factor(f) => AstTrait::take(f),
        }
    }

    fn replace(&mut self, ast: Ast) -> Ast {
        match self {
            Expr::BinaryOp(b) => AstTrait::replace(b, ast),
            Expr::Factor(f) => AstTrait::replace(f, ast),
        }
    }
}
