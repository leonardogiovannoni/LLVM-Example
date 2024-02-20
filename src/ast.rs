use crate::*;

use anyhow::Result;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct BinaryOp {
    pub lhs_expr: usize,
    pub rhs_expr: usize,
    pub op: Operator,
}

impl BinaryOp {
    pub fn new(lhs_expr: usize, rhs_expr: usize, op: Operator) -> BinaryOp {
        BinaryOp {
            lhs_expr,
            rhs_expr,
            op,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueKind {
    Ident,
    Number,
}

#[derive(Debug)]
pub struct Factor {
    pub kind: ValueKind,
    pub text: RefStr,
}

impl Factor {
    pub fn new(v: ValueKind, text: RefStr) -> Factor {
        Factor { kind: v, text }
    }
}

#[derive(Debug)]
pub struct WithDecl {
    pub vars: Vec<RefStr>,
    pub text: RefStr,
    pub expr: usize,
}

impl WithDecl {
    pub fn new(vars: Vec<RefStr>, text: RefStr, expr: usize) -> Self {
        WithDecl { vars, text, expr }
    }

    pub fn vars_iter(&self) -> impl Iterator<Item = &str> {
        self.vars.iter().map(|v| v.as_str())
    }
}

#[derive(Debug)]
pub enum Expr {
    BinaryOp(BinaryOp),
    Factor(Factor),
}

#[derive(Debug)]
pub enum Ast {
    WithDecl(WithDecl),
    Expr(usize),
}

pub trait AstTrait {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()>;
}

impl AstTrait for Ast {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        match self {
            Ast::WithDecl(ast) => ast.accept(v),
            Ast::Expr(ast) => ast.accept(v),
        }
    }
}

impl AstTrait for WithDecl {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit_with_decl(self)
    }
}

impl AstTrait for usize {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit_index(*self)
    }
}
