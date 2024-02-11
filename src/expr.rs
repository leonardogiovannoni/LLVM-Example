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

impl<'a> ASTTrait<'a> for Expr<'a> {
    fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn ASTVisitorTrait<'a>) -> Result<()> {
        match self {
            Expr::BinaryOp(ast) => ASTTrait::accept(ast, exprs, v),
            Expr::Factor(ast) => ASTTrait::accept(ast, exprs, v),
        }
    }

    fn swap(&mut self, ast: &mut AST<'a>) {
        match self {
            Expr::BinaryOp(b) => {
                ASTTrait::swap(b, ast);
            }
            Expr::Factor(f) => {
                ASTTrait::swap(f, ast);
            }
        }
    }

    fn take(&mut self) -> AST<'a> {
        match self {
            Expr::BinaryOp(b) => ASTTrait::take(b),
            Expr::Factor(f) => ASTTrait::take(f),
        }
    }

    fn replace(&mut self, ast: AST<'a>) -> AST<'a> {
        match self {
            Expr::BinaryOp(b) => ASTTrait::replace(b, ast),
            Expr::Factor(f) => ASTTrait::replace(f, ast),
        }
    }
}


