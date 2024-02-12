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

#[derive(Debug, Default, Clone)]
pub struct Factor<'a> {
    pub kind: ValueKind,
    pub val: &'a [char],
}

impl<'a> Factor<'a> {
    pub fn new(v: ValueKind, text: &'a [char]) -> Factor {
        Factor { kind: v, val: text }
    }
}

#[derive(Debug, Default)]
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
pub enum Ast<'a> {
    BinaryOp(BinaryOp),
    Factor(Factor<'a>),
    WithDecl(WithDecl<'a>),
    Index(ExprIndex),
}

#[enum_dispatch(Ast)]
pub trait AstTrait<'a> {
    fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn AstVisitorTrait<'a>) -> Result<()>;
    fn swap(&mut self, ast: &mut Ast<'a>);
    fn take(&mut self) -> Ast<'a>;
    fn replace(&mut self, ast: Ast<'a>) -> Ast<'a>;
}

macro_rules! impl_ast {
    ($($t:ty),*) => {
        $(
            impl<'a> AstTrait<'a> for $t {

                fn accept(&mut self, exprs: &mut Vec<Expr<'a>>, v: &mut dyn AstVisitorTrait<'a>) -> Result<()> {
                    v.visit(exprs, self)
                }

                fn swap(&mut self, ast: &mut Ast<'a>) {
                    let s = std::mem::take(self);
                    let ss = Ast::from(s);
                    let a = std::mem::take(ast);
                    *self = a.try_into().unwrap();
                    *ast = ss;
                }

                fn take(&mut self) -> Ast<'a> {
                    let mut ast = Ast::from(<$t>::default());
                    let s = std::mem::take(self);
                    let a = std::mem::take(&mut ast);
                    *self = a.try_into().unwrap();
                    s.into()
                }

                fn replace(&mut self, ast: Ast<'a>) -> Ast<'a> {
                    let s = std::mem::take(self);
                    *self = ast.try_into().unwrap();
                    s.into()
                }
            }
        )*
    };
}

impl_ast!(BinaryOp, Factor<'a>, WithDecl<'a>, ExprIndex);

impl<'a> Default for Ast<'a> {
    fn default() -> Self {
        Ast::Index(ExprIndex::default())
    }
}
