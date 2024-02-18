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

    fn format_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::BinaryOp(bin_op) => {
                let resolve_fn = |idx: Option<&Rc<Expr>>| {
                    idx.map(|idx| self.format_expr(idx))
                        .map(|expr| format!("Some({})", expr))
                        .unwrap_or_else(|| "None".to_string())
                };
                let lhs = resolve_fn(bin_op.lhs_expr.as_ref());
                let rhs = resolve_fn(bin_op.rhs_expr.as_ref());
                let op = format!("{:?}", bin_op.op);
                format!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op)
            }
            Expr::Factor(factor) => {
                let val = factor.text.iter().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                format!("Factor(kind: {}, val: {})", kind, val)
            }
        }
    }
}

impl<'a> AstVisitorTrait<'a> for DebugAstVisitor<'a> {
    fn visit_binary_op(&self, bin_op: &BinaryOp) -> Result<()> {
        let get_pretty_name = |idx: Option<&Rc<Expr>>| {
            idx.map(|idx| self.format_expr(idx))
                .map(|expr| format!("Some({})", expr))
                .unwrap_or_else(|| "None".to_string())
        };
        let lhs = get_pretty_name(bin_op.lhs_expr.as_ref());
        let rhs = get_pretty_name(bin_op.rhs_expr.as_ref());
        let op = format!("{:?}", bin_op.op);
        println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
        Ok(())
    }

    fn visit_factor(&self, factor: &Factor) -> Result<()> {
        let val = factor.text.iter().collect::<String>();
        let kind = format!("{:?}", factor.kind);
        println!("Factor(kind: {}, val: {})", kind, val);
        Ok(())
    }

    fn visit_with_decl(&self, with_decl: &WithDecl) -> Result<()> {
        let vars = with_decl
            .vars_iter()
            .map(|v| v.iter().collect::<String>())
            .collect::<Vec<_>>();
        let expr = with_decl
            .expr_index
            .as_ref()
            .map(|idx| self.format_expr(idx));
        let expr = expr
            .map(|expr| format!("Some({})", expr))
            .unwrap_or_else(|| "None".to_string());
        let vars = format!("{:?}", vars);
        println!("WithDecl(vars: {}, expr: {})", vars, expr);
        Ok(())
    }

    fn visit_index(&self, index: &Rc<Expr>) -> Result<()> {
        let index = format!("{:?}", index);
        println!("Index({})", index);
        Ok(())
    }

    fn visit(&self, ast: &Ast) -> Result<()> {
        match ast {
            Ast::BinaryOp(bin_op) => self.visit_binary_op(bin_op),
            Ast::Factor(factor) => self.visit_factor(factor),
            Ast::WithDecl(with_decl) => self.visit_with_decl(with_decl),
            Ast::Index(index) => self.visit_index(index),
        }
    }
}

pub fn debug_ast(ast: &Ast) {
    let visitor = DebugAstVisitor::new();
    visitor.visit(ast).unwrap();
}
