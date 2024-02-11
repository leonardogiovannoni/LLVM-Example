use crate::*;

use enum_dispatch::enum_dispatch;
use anyhow::Result;

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

#[derive(Debug, Clone, Default)]
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

#[derive(Debug, Clone, Default)]
pub struct Factor<'a> {
    pub kind: ValueKind,
    pub val: &'a [char],
}

impl<'a> Factor<'a> {
    pub fn new(v: ValueKind, text: &'a [char]) -> Factor {
        Factor { kind: v, val: text }
    }
}

#[derive(Debug, Clone, Default)]
pub struct WithDecl<'a> {
    pub vars: Vec<&'a [char]>,
    pub expr_index: Option<ExprIndex>,
}

impl<'a> WithDecl<'a> {
    pub fn new(vars: Vec<&'a [char]>, expr_index: Option<ExprIndex>) -> WithDecl<'a> {
        WithDecl { vars, expr_index }
    }

    pub fn get_expr(&self) -> Option<ExprIndex> {
        self.expr_index
    }
}

#[derive(Debug)]
#[enum_dispatch]
pub enum AST<'a> {
    BinaryOp(BinaryOp),
    Factor(Factor<'a>),
    WithDecl(WithDecl<'a>),
    Index(ExprIndex),
}

#[enum_dispatch(AST)]
pub trait ASTTrait<'a> {
    fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn ASTVisitorTrait<'a>) -> Result<()>;
    fn swap(&mut self, ast: &mut AST<'a>);
    fn take(&mut self) -> AST<'a>;
    fn replace(&mut self, ast: AST<'a>) -> AST<'a>;
}

macro_rules! impl_ast {
    ($($t:ty),*) => {
        $(
            impl<'a> ASTTrait<'a> for $t {

                fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn ASTVisitorTrait<'a>) -> Result<()> {
                    v.visit(exprs, self)
                }

                fn swap(&mut self, ast: &mut AST<'a>) {
                    let s = std::mem::take(self);
                    let ss = AST::from(s);
                    let a = std::mem::take(ast);
                    *self = a.try_into().unwrap();
                    *ast = ss;
                }

                fn take(&mut self) -> AST<'a> {
                    let mut ast = AST::from(<$t>::default());
                    let s = std::mem::take(self);
                    let a = std::mem::take(&mut ast);
                    *self = a.try_into().unwrap();
                    s.into()
                }

                fn replace(&mut self, ast: AST<'a>) -> AST<'a> {
                    let s = std::mem::take(self);
                    *self = ast.try_into().unwrap();
                    s.into()
                }
            }
        )*
    };
}

impl_ast!(BinaryOp, Factor<'a>, WithDecl<'a>, ExprIndex);

impl<'a> Default for AST<'a> {
    fn default() -> Self {
        AST::Index(ExprIndex::default())
    }
}
