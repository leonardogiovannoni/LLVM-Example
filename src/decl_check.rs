use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
};

use anyhow::{bail, Result};
use refslice::RefSlice;

use crate::{
    Ast, AstTrait, AstVisitorTrait, BinaryOp, ExprIndex, Factor, State, ValueKind, WithDecl,
};

#[derive(Debug)]
pub struct DeclCheck {
    pub scope: RefCell<HashSet<RefSlice<char>>>,
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

    pub fn error(&self, err: ErrorType, s: RefSlice<char>) {
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
    fn visit_binary_op(&self, state: &State, ast: &BinaryOp) -> Result<()> {
        let lhs = ast
            .lhs_expr
            .ok_or_else(|| anyhow::anyhow!("lhs_expr is None"))?;
        let rhs = ast
            .rhs_expr
            .ok_or_else(|| anyhow::anyhow!("rhs_expr is None"))?;
        lhs.accept(state, self)?;
        rhs.accept(state, self)?;
        Ok(())
    }

    fn visit_factor(&self, _state: &State, ast: &Factor) -> Result<()> {
        if ast.kind == ValueKind::Ident && !self.scope.borrow().contains(&ast.text) {
            self.error(ErrorType::Not, ast.text.index(..));
        }
        Ok(())
    }

    #[inline(never)]
    fn visit_with_decl(&self, _state: &State, ast: &WithDecl) -> Result<()> {
        for i in ast.vars.iter() {
            if self.scope.borrow().contains(i) {
                self.error(ErrorType::Twice, i.index(..));
                bail!("Variable declared twice");
            }
            self.scope.borrow_mut().insert(i.index(..));
        }
        Ok(())
    }

    fn visit_index(&self, state: &State, ast: &ExprIndex) -> Result<()> {
        let exprs = &state.exprs;
       // let e = &tmp[ast.0];
        let e = exprs.get(*ast).unwrap();
        let e = e.borrow();
        e.accept(state, self)
    }

    fn visit(&self, state: &State, ast: &Ast) -> Result<()> {
        match ast {
            Ast::BinaryOp(bin_op) => self.visit_binary_op(state, bin_op),
            Ast::Factor(factor) => self.visit_factor(state, factor),
            Ast::WithDecl(with_decl) => self.visit_with_decl(state, with_decl),
            Ast::Index(index) => self.visit_index(state, index),
        }
    }
}
