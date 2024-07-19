use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
    rc::Rc,
};

use crate::util::{RcStr, Span};
use crate::{Ast, AstTrait, AstVisitorTrait, BinaryOp, Expr, Factor, ValueKind, WithDecl};
use anyhow::{bail, Result};

#[derive(Debug)]
pub struct DeclCheck {
    pub scope: RefCell<HashSet<RcStr>>,
    pub text: Rc<str>,
    pub has_error: Cell<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorType {
    Twice,
    Not,
}

impl DeclCheck {
    pub fn new(text: Rc<str>) -> Self {
        Self {
            scope: RefCell::new(HashSet::new()),
            text,
            has_error: Cell::new(false),
        }
    }

    #[inline(always)]
    fn visit_binary_op(&self, ast: &BinaryOp) -> Result<()> {
        ast.lhs_expr.accept(self)?;
        ast.rhs_expr.accept(self)?;
        Ok(())
    }

    #[inline(always)]
    fn visit_factor(&self, ast: &Factor) -> Result<()> {
        let tmp = RcStr::new(Rc::clone(&self.text), ast.span);
        if ast.kind == ValueKind::Ident && !self.scope.borrow().contains(&tmp) {
            self.error(ErrorType::Not, ast.span);
        }
        Ok(())
    }

    pub fn error(&self, err: ErrorType, s: Span) {
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
        for &span in ast.vars.iter() {
            let tmp = RcStr::new(Rc::clone(&self.text), span);
            if self.scope.borrow().contains(&tmp) {
                self.error(ErrorType::Twice, span);
                bail!("Variable declared twice");
            }
            self.scope.borrow_mut().insert(tmp);
        }
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::BinaryOp(bin_op) => self.visit_binary_op(bin_op),
            Expr::Factor(factor) => self.visit_factor(factor),
        }
    }

    fn visit(&self, ast: &Ast) -> Result<()> {
        match ast {
            Ast::WithDecl(with_decl) => self.visit_with_decl(with_decl),
            Ast::Expr(expr) => self.visit_expr(expr),
        }
    }
}
