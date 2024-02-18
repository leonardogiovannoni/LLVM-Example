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

    fn resolve_and_format_expr_index(&self, exprs: &State, index: ExprIndex) -> String {
        //let expr = &exprs[index.0];
        //self.format_expr(exprs, expr)
        let expr = exprs.exprs.get(index).unwrap();
        let expr = expr.borrow();
        self.format_expr(exprs, &*expr)
    }

    fn format_expr(&self, exprs: &State, expr: &Expr) -> String {
        match expr {
            Expr::BinaryOp(bin_op) => {
                let resolve_fn = |idx: Option<ExprIndex>| {
                    idx.map(|idx| self.resolve_and_format_expr_index(exprs, idx))
                        .map(|expr| format!("Some({})", expr))
                        .unwrap_or_else(|| "None".to_string())
                };
                let lhs = resolve_fn(bin_op.lhs_expr);
                let rhs = resolve_fn(bin_op.rhs_expr);
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
    fn visit_binary_op(&self, state: &State, bin_op: &BinaryOp) -> Result<()> {
        let get_pretty_name = |idx: Option<ExprIndex>| {
            idx.map(|idx| self.resolve_and_format_expr_index(state, idx))
                .map(|expr| format!("Some({})", expr))
                .unwrap_or_else(|| "None".to_string())
        };
        let lhs = get_pretty_name(bin_op.lhs_expr);
        let rhs = get_pretty_name(bin_op.rhs_expr);
        let op = format!("{:?}", bin_op.op);
        println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
        Ok(())
    }

    fn visit_factor(&self, _state: &State, factor: &Factor) -> Result<()> {
        let val = factor.text.iter().collect::<String>();
        let kind = format!("{:?}", factor.kind);
        println!("Factor(kind: {}, val: {})", kind, val);
        Ok(())
    }

    fn visit_with_decl(&self, state: &State, with_decl: &WithDecl) -> Result<()> {
        let vars = with_decl
            .vars_iter()
            .map(|v| v.iter().collect::<String>())
            .collect::<Vec<_>>();
        let expr = with_decl
            .expr_index
            .map(|idx| self.resolve_and_format_expr_index(state, idx));
        let expr = expr
            .map(|expr| format!("Some({})", expr))
            .unwrap_or_else(|| "None".to_string());
        let vars = format!("{:?}", vars);
        println!("WithDecl(vars: {}, expr: {})", vars, expr);
        Ok(())
    }

    fn visit_index(&self, _state: &State, index: &ExprIndex) -> Result<()> {
        let index = format!("{:?}", index.0);
        println!("Index({})", index);
        Ok(())
    }

    fn visit(&self, exprs: &State, ast: &Ast) -> Result<()> {
        match ast {
            Ast::BinaryOp(bin_op) => self.visit_binary_op(exprs, bin_op),
            Ast::Factor(factor) => self.visit_factor(exprs, factor),
            Ast::WithDecl(with_decl) => self.visit_with_decl(exprs, with_decl),
            Ast::Index(index) => self.visit_index(exprs, index),
        }
    }
}

pub fn debug_ast(ast: &Ast, state: &State) {
    let visitor = DebugAstVisitor::new();
    visitor.visit(state, ast).unwrap();
}
