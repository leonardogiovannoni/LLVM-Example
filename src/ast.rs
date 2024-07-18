use crate::*;
use crate::util::Span;
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
    pub lhs_expr: Box<Expr>,
    pub rhs_expr: Box<Expr>,
    pub op: Operator,
}

impl BinaryOp {
    pub fn new(lhs_expr: Expr, rhs_expr: Expr, op: Operator) -> BinaryOp {
        BinaryOp {
            lhs_expr: Box::new(lhs_expr),
            rhs_expr: Box::new(rhs_expr),
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
    pub span: Span,
}

impl Factor {
    pub fn new(v: ValueKind, text: Span) -> Factor {
        Factor { kind: v, span: text }
    }
}

#[derive(Debug)]
pub struct WithDecl {
    pub vars: Vec<Span>,
    pub expr: Box<Expr>,
}

impl WithDecl {
    pub fn new(vars: Vec<Span>, expr: Expr) -> Self {
        WithDecl {
            vars,
            expr: Box::new(expr),
        }
    }

    pub fn vars_iter<'a>(&'a self, text: &'a str) -> impl Iterator<Item = &'a str> {
        self.vars.iter().map(|v| &text[v.begin..v.end])
    }
}

#[derive(Debug)]
pub enum Expr {
    BinaryOp(BinaryOp),
    Factor(Factor),
}

#[derive(Debug)]
pub enum Ast {
    WithDecl(Box<WithDecl>),
    Expr(Box<Expr>),
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

impl AstTrait for Expr {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit_expr(self)
    }
}
