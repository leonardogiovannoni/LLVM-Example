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
    pub text: Rc<[char]>,
    pub span: Span,
}

impl Default for Factor {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            text: Rc::new([]),
            span: Span::new(0, 0),
        }
    }
}

impl Factor {
    pub fn new(v: ValueKind, text: Rc<[char]>, span: Span) -> Factor {
        Factor {
            kind: v,
            text,
            span,
        }
    }
}

#[derive(Debug)]
pub struct WithDecl {
    pub vars: Vec<Span>,
    pub text: Rc<[char]>,
    pub expr_index: Option<ExprIndex>,
}

impl Default for WithDecl {
    fn default() -> Self {
        Self {
            vars: Default::default(),
            text: Rc::new([]),
            expr_index: Default::default(),
        }
    }
}

impl WithDecl {
    pub fn new(vars: Vec<Span>, text: Rc<[char]>, expr_index: Option<ExprIndex>) -> WithDecl {
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
        self.vars
            .iter()
            .map(|v| self.text.get(v.start..v.end).unwrap())
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
    fn accept<'a>(&mut self, exprs: &mut Vec<Expr>, v: &dyn AstVisitorTrait<'a>) -> Result<()>;
    fn swap(&mut self, ast: &mut Ast);
    fn take(&mut self) -> Ast;
    fn replace(&mut self, ast: Ast) -> Ast;
}

macro_rules! impl_ast {
    ($($t:ty),*) => {
        $(
            impl AstTrait for $t {

                fn accept<'a>(&mut self, exprs: &mut Vec<Expr>, v: &dyn AstVisitorTrait<'a>) -> Result<()> {
                    v.visit(exprs, self)
                }

                fn swap(&mut self, ast: &mut Ast) {
                    let s = std::mem::take(self);
                    let ss = Ast::from(s);
                    let a = std::mem::take(ast);
                    *self = a.try_into().unwrap();
                    *ast = ss;
                }

                fn take(&mut self) -> Ast {
                    let mut ast = Ast::from(<$t>::default());
                    let s = std::mem::take(self);
                    let a = std::mem::take(&mut ast);
                    *self = a.try_into().unwrap();
                    s.into()
                }

                fn replace(&mut self, ast: Ast) -> Ast {
                    let s = std::mem::take(self);
                    *self = ast.try_into().unwrap();
                    s.into()
                }
            }
        )*
    };
}

impl_ast!(BinaryOp, Factor, WithDecl, ExprIndex);

impl<'a> Default for Ast {
    fn default() -> Self {
        Ast::Index(ExprIndex::default())
    }
}
