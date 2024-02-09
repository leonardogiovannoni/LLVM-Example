use enum_dispatch::enum_dispatch;
use inkwell::{
    builder::Builder,
    module::Module,
    types::{IntType, PointerType, VoidType},
    values::{AnyValue, AnyValueEnum, IntMathValue, IntValue},
};

use crate::*;

#[derive(Debug)]
#[enum_dispatch]
pub enum ASTVisitor<'a> {
    DeclCheck(DeclCheck<'a>),
    ToIRVisitor(ToIRVisitor<'a>),
}

impl<'a> Default for ASTVisitor<'a> {
    fn default() -> Self {
        ASTVisitor::DeclCheck(DeclCheck::new())
    }
}

#[enum_dispatch(ASTVisitor)]
pub trait ASTVisitorTrait<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>);
    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn ASTTrait<'a>);
}


#[derive(Debug)]
pub struct ToIRVisitor<'ctx> {
    m: Module<'ctx>,
    ir_builder: Builder<'ctx>,
    void_type: VoidType<'ctx>,
    int32_type: IntType<'ctx>,
    pointer_type: PointerType<'ctx>,
    int32_zero: IntValue<'ctx>,
    v: AnyValueEnum<'ctx>,
    phantom: std::marker::PhantomData<&'ctx ()>,
    /*value */
    /*name_map */
}

impl<'a> Default for ToIRVisitor<'a> {
    fn default() -> Self {
        todo!()
    }
}

impl<'a> ASTVisitorTrait<'a> for ToIRVisitor<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>) {
        let mut this = std::mem::take(self);
        let ast_owned = std::mem::take(ast);
        match ast_owned {
            AST::BinaryOp(node) => {
                let Some(mut left) = node.lhs_expr else {
                    panic!("left is none");
                };
                let l = self.v;
                left.accept(exprs, &mut this);
                let Some(right) = node.rhs_expr else {
                    panic!("right is none");
                };
                let r = this.v;

                macro_rules! f {
                    ($f:expr) => {
                        match (l, r) {
                            (AnyValueEnum::IntValue(l), AnyValueEnum::IntValue(r)) => {
                                let v = $f(&self.ir_builder, l, r, "addtmp").unwrap();
                                self.v = v.as_any_value_enum();
                            }
                            (AnyValueEnum::VectorValue(l), AnyValueEnum::VectorValue(r)) => {
                                let v = $f(&self.ir_builder, l, r, "addtmp").unwrap();
                                self.v = v.as_any_value_enum();
                            }
                            (AnyValueEnum::PointerValue(l), AnyValueEnum::PointerValue(r)) => {
                                let v = $f(&self.ir_builder, l, r, "addtmp").unwrap();
                                self.v = v.as_any_value_enum();
                            }
                            _ => panic!("not int"),
                        }
                    };
                }

                match node.op {
                    Operator::Plus => f!(Builder::build_int_nsw_add),
                    Operator::Minus => f!(Builder::build_int_nsw_sub),
                    Operator::Mul => f!(Builder::build_int_nsw_mul),
                    Operator::Div => f!(Builder::build_int_signed_div),
                }
            }
            AST::Factor(node) => {
                if node.kind == ValueKind::Ident {
                    //if self.scope.contains(node.val) {
                    //     return (self.error(ErrorType::Twice, node.val), AST::Factor(node));
                    //}
                }
            }
            AST::WithDecl(mut node) => {
                let read_fty = self.int32_type.fn_type(&[], false);
                // let read_fn = self.m.add_function("calc_read", read_fty, GlobalValue::ExternalLinkage);
                for &i in &node.vars {
                   // let str_text = self.m.get_context().const_string(i.as_bytes(), true);
                    // ADD GLOBAL
                }
            }
            AST::Index(index) => {
                todo!()
                //let expr = exprs.get(index).unwrap();
                //expr.accept(self);
                //(self, AST::Index(index))
            }
        }
        //  *self = this;
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn ASTTrait<'a>) {
        let mut tmp = ast.take_ast();
        self.inner_visit(exprs, &mut tmp);
        let _ = ast.replace(tmp);
    }
}

#[derive(Debug, Default)]
pub struct DeclCheck<'a> {
    scope: HashSet<&'a [char]>,
    has_error: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorType {
    Twice,
    Not,
}

impl<'a> DeclCheck<'a> {
    pub fn new() -> DeclCheck<'a> {
        DeclCheck {
            scope: HashSet::new(),
            has_error: false,
        }
    }

    pub fn error(&mut self, err: ErrorType, s: &[char]) {
        let tmp = if err == ErrorType::Twice {
            "twice"
        } else {
            "not"
        };
        println!("Variable {:?} is {} declared", s, tmp);
        self.has_error = true;
    }

    pub fn check(&self, _ast: AST<'a>) -> bool {
        // let ast = ast.accept(self);
        self.has_error
    }
}

impl<'a> ASTVisitorTrait<'a> for DeclCheck<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, node: &mut AST<'a>) {
        match node {
            AST::BinaryOp(node) => {
                // TODO: check for existence of ExprIndex in exprs
                if let Some(mut left) = node.lhs_expr {
                    self.visit(exprs, &mut left);
                } else {
                    self.has_error = true;
                }
                if let Some(mut right) = node.rhs_expr {
                    self.visit(exprs, &mut right);
                } else {
                    self.has_error = true;
                }
            }
            AST::Factor(node) => {
                if node.kind == ValueKind::Ident && !self.scope.contains(node.val) {
                    self.error(ErrorType::Twice, node.val);
                }
            }
            AST::WithDecl(node) => {
                for &i in &node.vars {
                    if self.scope.contains(i) {
                        self.error(ErrorType::Twice, i);
                        return;
                    }
                    self.scope.insert(i);
                }

                if let Some(mut expr) = node.expr_index {
                    self.visit(exprs, &mut expr);
                } else {
                    self.has_error = true;
                }
            }
            AST::Index(index) => {
                if let Some(expr) = exprs.get_mut(index.0) {
                    let tmp = std::mem::take(expr);
                    exprs[index.0] = match tmp {
                        Expr::BinaryOp(mut node) => {
                            self.visit(exprs, &mut node);
                            node.into()
                        }
                        Expr::Factor(mut node) => {
                            self.visit(exprs, &mut node);
                            node.into()
                        }
                    };
                } else {
                    self.has_error = true;
                }
            }
        }
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn ASTTrait<'a>) {
        let mut tmp = ast.take_ast();
        self.inner_visit(exprs, &mut tmp);
        let _ = ast.replace(tmp);
    }
}






struct DebugASTVisitor<'a> {
    phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> DebugASTVisitor<'a> {

    fn new() -> DebugASTVisitor<'a> {
        DebugASTVisitor {
            phantom: std::marker::PhantomData,
        }
    }

    fn resolve_and_format_expr_index(&self, exprs: &[Expr<'a>], index: ExprIndex) -> String {
        let expr = &exprs[index.0];
        self.format_expr(exprs, expr)
    }

    fn format_expr(&self, exprs: &[Expr<'a>], expr: &Expr<'a>) -> String {
        match expr {
            Expr::BinaryOp(bin_op) => {
                let lhs = bin_op.lhs_expr.map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let rhs = bin_op.rhs_expr.map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let lhs = lhs.map(|lhs| format!("{}", lhs)).unwrap_or_else(|| "None".to_string());
                let rhs = rhs.map(|rhs| format!("{}", rhs)).unwrap_or_else(|| "None".to_string());
                let op = format!("{:?}", bin_op.op);
                format!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op)
            },
            Expr::Factor(factor) => {
                let val = factor.val.iter().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                format!("Factor(kind: {}, val: {})", kind, val)
            },
            
        }
    }
}


impl<'a> ASTVisitorTrait<'a> for DebugASTVisitor<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>) {
        match ast {
            AST::BinaryOp(bin_op) => {
                let lhs = bin_op.lhs_expr.map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let rhs = bin_op.rhs_expr.map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let lhs = lhs.map(|lhs| format!("{}", lhs)).unwrap_or_else(|| "None".to_string());
                let rhs = rhs.map(|rhs| format!("{}", rhs)).unwrap_or_else(|| "None".to_string());
                let op = format!("{:?}", bin_op.op);
                println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
            },
            AST::Factor(factor) => {
                let val = factor.val.iter().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                println!("Factor(kind: {}, val: {})", kind, val);
            },
            AST::WithDecl(with_decl) => {
                let vars = with_decl.vars.iter().map(|v| v.iter().collect::<String>()).collect::<Vec<_>>();
                let expr = with_decl.expr_index.map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let expr = expr.map(|expr| format!("{}", expr)).unwrap_or_else(|| "None".to_string());                
                let vars = format!("{:?}", vars);
                println!("WithDecl(vars: {}, expr: {})", vars, expr);
            },
            AST::Index(index) => {
                let index = format!("{:?}", index.0);
                println!("Index({})", index);
            },
        }
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn ASTTrait<'a>) {
        let mut tmp = ast.take_ast();
        self.inner_visit(exprs, &mut tmp);
        let _ = ast.replace(tmp);
    }
}

pub fn debug_ast<'a>(ast: &mut AST<'a>, exprs: &mut Vec<Expr<'a>>) {
    let mut visitor = DebugASTVisitor::new();
    visitor.visit(exprs, ast);
}

