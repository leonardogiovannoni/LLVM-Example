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

    fn callbacks_mut(&mut self,bin_op:Option<impl FnOnce(&mut BinaryOp) -> Result<()>>,factor:Option<impl FnOnce(&mut Factor) -> Result<()>>,with_decl:Option<impl FnOnce(&mut WithDecl) -> Result<()>>,index:Option<impl FnOnce(&mut ExprIndex) -> Result<()>>,) -> Result<()> {
        match self {
            Expr::BinaryOp(b) => {
                AstTrait::callbacks_mut(b, bin_op, factor, with_decl, index)
            }
            Expr::Factor(f) => {
                AstTrait::callbacks_mut(f, bin_op, factor, with_decl, index)
            }
        }
    }

    fn callbacks(&self,bin_op:Option<impl FnOnce(&BinaryOp) -> Result<()>>,factor:Option<impl FnOnce(&Factor) -> Result<()>>,with_decl:Option<impl FnOnce(&WithDecl) -> Result<()>>,index:Option<impl FnOnce(&ExprIndex) -> Result<()>>,) -> Result<()> {
        match self {
            Expr::BinaryOp(b) => {
                AstTrait::callbacks(b, bin_op, factor, with_decl, index)
            }
            Expr::Factor(f) => {
                AstTrait::callbacks(f, bin_op, factor, with_decl, index)
            }
        }
    }
}
