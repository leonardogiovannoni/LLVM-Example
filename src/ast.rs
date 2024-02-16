use crate::*;

use anyhow::Result;
use enum_dispatch::enum_dispatch;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ExprIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Operator {
    #[default]
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Debug, Default, Clone)]
pub struct BinaryOp {
    pub lhs_expr: Option<ExprIndex>,
    pub rhs_expr: Option<ExprIndex>,
    pub op: Operator,
}

impl BinaryOp {
    pub fn new(lhs_expr: Option<ExprIndex>, rhs_expr: Option<ExprIndex>, op: Operator) -> BinaryOp {
        BinaryOp {
            lhs_expr,
            rhs_expr,
            op,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ValueKind {
    #[default]
    Ident,
    Number,
}

#[derive(Debug, Clone)]
pub struct Factor {
    pub kind: ValueKind,
    pub text: RefSlice<char>,
}

impl Default for Factor {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            text: RefSlice::from([]),
        }
    }
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
    pub expr_index: Option<ExprIndex>,
}

impl Default for WithDecl {
    fn default() -> Self {
        Self {
            vars: Default::default(),
            text: RefSlice::from([]),
            expr_index: Default::default(),
        }
    }
}

impl WithDecl {
    pub fn new(
        vars: Vec<RefSlice<char>>,
        text: RefSlice<char>,
        expr_index: Option<ExprIndex>,
    ) -> WithDecl {
        WithDecl {
            vars,
            text,
            expr_index,
        }
    }

    pub fn get_expr(&self) -> Option<ExprIndex> {
        self.expr_index
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
    Index(ExprIndex),
}

#[enum_dispatch(Ast)]
pub trait AstTrait {
    fn accept<'a>(&self, exprs: &State, v: &impl AstVisitorTrait<'a>) -> Result<()>;

    fn callbacks(
        &self,
        bin_op: Option<impl FnOnce(&BinaryOp) -> Result<()>>,
        factor: Option<impl FnOnce(&Factor) -> Result<()>>,
        with_decl: Option<impl FnOnce(&WithDecl) -> Result<()>>,
        index: Option<impl FnOnce(&ExprIndex) -> Result<()>>,
    ) -> Result<()>;
}

impl Default for Ast {
    fn default() -> Self {
        Ast::Index(ExprIndex::default())
    }
}

impl AstTrait for BinaryOp {
    fn accept<'a>(&self, exprs: &State, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit(exprs, self)
    }

    fn callbacks(
        &self,
        bin_op: Option<impl FnOnce(&BinaryOp) -> Result<()>>,
        _factor: Option<impl FnOnce(&Factor) -> Result<()>>,
        _with_decl: Option<impl FnOnce(&WithDecl) -> Result<()>>,
        _index: Option<impl FnOnce(&ExprIndex) -> Result<()>>,
    ) -> Result<()> {
        bin_op.unwrap()(self)
    }
}

impl AstTrait for Factor {
    fn accept<'a>(&self, exprs: &State, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit(exprs, self)
    }

    fn callbacks(
        &self,
        _bin_op: Option<impl FnOnce(&BinaryOp) -> Result<()>>,
        factor: Option<impl FnOnce(&Factor) -> Result<()>>,
        _with_decl: Option<impl FnOnce(&WithDecl) -> Result<()>>,
        _index: Option<impl FnOnce(&ExprIndex) -> Result<()>>,
    ) -> Result<()> {
        factor.unwrap()(self)
    }
}

impl AstTrait for WithDecl {
    fn accept<'a>(&self, exprs: &State, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit(exprs, self)
    }

    fn callbacks(
        &self,
        _bin_op: Option<impl FnOnce(&BinaryOp) -> Result<()>>,
        _factor: Option<impl FnOnce(&Factor) -> Result<()>>,
        with_decl: Option<impl FnOnce(&WithDecl) -> Result<()>>,
        _index: Option<impl FnOnce(&ExprIndex) -> Result<()>>,
    ) -> Result<()> {
        with_decl.unwrap()(self)
    }
}

impl AstTrait for ExprIndex {
    fn accept<'a>(&self, exprs: &State, v: &impl AstVisitorTrait<'a>) -> Result<()> {
        v.visit(exprs, self)
    }

    fn callbacks(
        &self,
        _bin_op: Option<impl FnOnce(&BinaryOp) -> Result<()>>,
        _factor: Option<impl FnOnce(&Factor) -> Result<()>>,
        _with_decl: Option<impl FnOnce(&WithDecl) -> Result<()>>,
        index: Option<impl FnOnce(&ExprIndex) -> Result<()>>,
    ) -> Result<()> {
        index.unwrap()(self)
    }
}
