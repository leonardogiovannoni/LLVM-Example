use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
    rc::Rc,
};

use anyhow::{bail, Result};
use refslice::refstr::RefStr;

use crate::{Ast, AstTrait, AstVisitorTrait, BinaryOp, Expr, Factor, ValueKind, WithDecl, EXPR};

#[derive(Debug)]
pub struct DeclCheck {
    pub scope: RefCell<HashSet<RefStr>>,
    pub has_error: Cell<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorType {
    Twice,
    Not,
}

impl DeclCheck {
    pub fn new() -> Self {
        DeclCheck {
            scope: RefCell::new(HashSet::new()),
            has_error: Cell::new(false),
        }
    }

    fn visit_binary_op(&self, ast: &BinaryOp) -> Result<()> {
        ast.lhs_expr.accept(self)?;
        ast.rhs_expr.accept(self)?;
        Ok(())
    }

    fn visit_factor(&self, ast: &Factor) -> Result<()> {
        if ast.kind == ValueKind::Ident && !self.scope.borrow().contains(&ast.text) {
            self.error(ErrorType::Not, ast.text.index(..));
        }
        Ok(())
    }

    pub fn error(&self, err: ErrorType, s: RefStr) {
        let tmp = if err == ErrorType::Twice {
            "twice"
        } else {
            "not"
        };
        println!("Variable {:?} is {} declared", s, tmp);
        self.has_error.set(true);
    }
}

impl<'a> AstVisitorTrait<'a> for DeclCheck {
    fn visit_with_decl(&self, ast: &WithDecl) -> Result<()> {
        for i in ast.vars.iter() {
            if self.scope.borrow().contains(i) {
                self.error(ErrorType::Twice, i.index(..));
                bail!("Variable declared twice");
            }
            self.scope.borrow_mut().insert(i.index(..));
        }
        Ok(())
    }

    fn visit_index(&self, ast: usize) -> Result<()> {
        let expr = EXPR.get(ast).unwrap();
        match &*expr {
            Expr::BinaryOp(bin_op) => self.visit_binary_op(bin_op),
            Expr::Factor(factor) => self.visit_factor(factor),
        }
    }

    fn visit(&self, ast: &Ast) -> Result<()> {
        match ast {
            Ast::WithDecl(with_decl) => self.visit_with_decl(with_decl),
            Ast::Expr(index) => self.visit_index(*index),
        }
    }
}
