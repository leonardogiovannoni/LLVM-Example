use crate::*;
use anyhow::Result;
struct DebugAstVisitor<'a> {
    phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> DebugAstVisitor<'a> {
    fn new() -> DebugAstVisitor<'a> {
        DebugAstVisitor {
            phantom: std::marker::PhantomData,
        }
    }

    fn visit_binary_op(&self, bin_op: &BinaryOp) -> Result<()> {
        let get_pretty_name = |idx: usize| {
            Some(idx)
                .map(|idx| self.format_expr(&*EXPR.get(idx).unwrap()))
                .map(|expr| format!("Some({})", expr))
                .unwrap_or_else(|| "None".to_string())
        };
        let lhs = get_pretty_name(bin_op.lhs_expr);
        let rhs = get_pretty_name(bin_op.rhs_expr);
        let op = format!("{:?}", bin_op.op);
        println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
        Ok(())
    }

    fn visit_factor(&self, factor: &Factor) -> Result<()> {
        let val = factor.text.as_str().chars().collect::<String>();
        let kind = format!("{:?}", factor.kind);
        println!("Factor(kind: {}, val: {})", kind, val);
        Ok(())
    }

    fn format_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::BinaryOp(bin_op) => {
                let resolve_fn = |idx: usize| {
                    Some(idx)
                        .map(|idx| self.format_expr(&*EXPR.get(idx).unwrap()))
                        .map(|expr| format!("Some({})", expr))
                        .unwrap_or_else(|| "None".to_string())
                };
                let lhs = resolve_fn(bin_op.lhs_expr);
                let rhs = resolve_fn(bin_op.rhs_expr);
                let op = format!("{:?}", bin_op.op);
                format!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op)
            }
            Expr::Factor(factor) => {
                let val = factor.text.as_str().chars().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                format!("Factor(kind: {}, val: {})", kind, val)
            }
        }
    }
}

impl<'a> AstVisitorTrait<'a> for DebugAstVisitor<'a> {
    fn visit_with_decl(&self, with_decl: &WithDecl) -> Result<()> {
        let vars = with_decl
            .vars_iter()
            .map(|v| v.to_owned())
            .collect::<Vec<_>>();

        let tmp = EXPR.get(with_decl.expr).unwrap();
        let expr = self.format_expr(&*tmp);
        let vars = format!("{:?}", vars);
        println!("WithDecl(vars: {}, expr: {})", vars, expr);
        Ok(())
    }

    fn visit_index(&self, index: usize) -> Result<()> {
        let index = EXPR.get(index).unwrap();
        match &*index {
            Expr::BinaryOp(bin_op) => self.visit_binary_op(bin_op),
            Expr::Factor(factor) => self.visit_factor(factor),
        }
    }

    fn visit(&self, ast: &Ast) -> Result<()> {
        match ast {
            Ast::WithDecl(with_decl) => self.visit_with_decl(with_decl),
            Ast::Expr(index) => self.visit_index(*index),
        }
    }
}

pub fn debug_ast(ast: &Ast) {
    let visitor = DebugAstVisitor::new();
    visitor.visit(ast).unwrap();
}
