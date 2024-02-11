use crate::*;
use enum_dispatch::enum_dispatch;
use anyhow::Result;
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

impl<'a> AstTrait<'a> for Expr<'a> {
    fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn AstVisitorTrait<'a>) -> Result<()> {
        match self {
            Expr::BinaryOp(ast) => AstTrait::accept(ast, exprs, v),
            Expr::Factor(ast) => AstTrait::accept(ast, exprs, v),
        }
    }

    fn swap(&mut self, ast: &mut Ast<'a>) {
        match self {
            Expr::BinaryOp(b) => {
                AstTrait::swap(b, ast);
            }
            Expr::Factor(f) => {
                AstTrait::swap(f, ast);
            }
        }
    }

    fn take(&mut self) -> Ast<'a> {
        match self {
            Expr::BinaryOp(b) => AstTrait::take(b),
            Expr::Factor(f) => AstTrait::take(f),
        }
    }

    fn replace(&mut self, ast: Ast<'a>) -> Ast<'a> {
        match self {
            Expr::BinaryOp(b) => AstTrait::replace(b, ast),
            Expr::Factor(f) => AstTrait::replace(f, ast),
        }
    }
}


