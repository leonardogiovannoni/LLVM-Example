use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
};

use crate::util::Span;
use crate::{Ast, AstTrait, AstVisitorTrait, BinaryOp, Expr, Factor, ValueKind, WithDecl};
use anyhow::{bail, Result};

#[derive(Debug)]
pub struct DeclCheck<'a> {
    //pub scope: RefCell<HashSet<RcStr>>,
    pub scope: RefCell<HashSet<&'a str>>,
    pub text: &'a str,
    pub has_error: Cell<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorType {
    Twice,
    Not,
}

impl<'a> DeclCheck<'a> {
    pub fn new(text: &'a str) -> Self {
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
        let tmp = &self.text[ast.span.begin..ast.span.end];
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

impl<'a> AstVisitorTrait<'a> for DeclCheck<'a> {
    fn visit_with_decl(&self, ast: &WithDecl) -> Result<()> {
        for &span in ast.vars.iter() {
            let tmp = &self.text[span.begin..span.end];
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
