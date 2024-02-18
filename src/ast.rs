use crate::*;

use anyhow::Result;
use enum_dispatch::enum_dispatch;



#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct BinaryOp {
    pub lhs_expr: Option<Rc<Expr>>,
    pub rhs_expr: Option<Rc<Expr>>,
    pub op: Operator,
}

impl BinaryOp {
    pub fn new(lhs_expr: Option<Rc<Expr>>, rhs_expr: Option<Rc<Expr>>, op: Operator) -> BinaryOp {
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
    pub text: RefSlice<char>,
}

impl Factor {
    pub fn new(v: ValueKind, text: RefSlice<char>) -> Factor {
        Factor { kind: v, text }
    }
}

#[derive(Debug)]
pub struct WithDecl {
    pub vars: Vec<RefSlice<char>>,
    pub text: RefSlice<char>,
    pub expr_index: Option<Rc<Expr>>,
}

impl WithDecl {
    pub fn new(
        vars: Vec<RefSlice<char>>,
        text: RefSlice<char>,
        expr_index: Option<Rc<Expr>>,
    ) -> WithDecl {
        WithDecl {
            vars,
            text,
            expr_index,
        }
    }

    pub fn get_expr(&self) -> Option<Rc<Expr>> {
        self.expr_index.clone()
    }

    pub fn vars_iter(&self) -> impl Iterator<Item = &[char]> {
        self.vars.iter().map(|v| v.as_ref())
    }
}

#[derive(Debug)]
#[enum_dispatch]
pub enum Ast {
    BinaryOp(BinaryOp),
    Factor(Factor),
    WithDecl(WithDecl),
    Index(Rc<Expr>),
}

#[enum_dispatch(Ast)]
pub trait AstTrait {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()>;
}

impl AstTrait for BinaryOp {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit_binary_op(self)
    }
}

impl AstTrait for Factor {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit_factor(self)
    }
}

impl AstTrait for WithDecl {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit_with_decl(self)
    }
}

impl AstTrait for Rc<Expr> {
    fn accept<'a>(&self, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit_index(self)
    }
}
