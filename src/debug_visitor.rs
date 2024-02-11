use crate::*;
use anyhow::Result;
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
                let lhs = bin_op
                    .lhs_expr
                    .map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let rhs = bin_op
                    .rhs_expr
                    .map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let lhs = lhs
                    .map(|lhs| format!("Some({})", lhs))
                    .unwrap_or_else(|| "None".to_string());
                let rhs = rhs
                    .map(|rhs| format!("Some({})", rhs))
                    .unwrap_or_else(|| "None".to_string());
                let op = format!("{:?}", bin_op.op);
                format!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op)
            }
            Expr::Factor(factor) => {
                let val = factor.val.iter().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                format!("Factor(kind: {}, val: {})", kind, val)
            }
        }
    }
}

impl<'a> ASTVisitorTrait<'a> for DebugASTVisitor<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>) -> Result<()> {
        match ast {
            AST::BinaryOp(bin_op) => {
                let lhs = bin_op
                    .lhs_expr
                    .map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let rhs = bin_op
                    .rhs_expr
                    .map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let lhs = lhs
                    .map(|lhs| format!("Some({})", lhs))
                    .unwrap_or_else(|| "None".to_string());
                let rhs = rhs
                    .map(|rhs| format!("Some({})", rhs))
                    .unwrap_or_else(|| "None".to_string());
                let op = format!("{:?}", bin_op.op);
                println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
            }
            AST::Factor(factor) => {
                let val = factor.val.iter().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                println!("Factor(kind: {}, val: {})", kind, val);
            }
            AST::WithDecl(with_decl) => {
                let vars = with_decl
                    .vars
                    .iter()
                    .map(|v| v.iter().collect::<String>())
                    .collect::<Vec<_>>();
                let expr = with_decl
                    .expr_index
                    .map(|idx| self.resolve_and_format_expr_index(exprs, idx));
                let expr = expr
                    .map(|expr| format!("Some({})", expr))
                    .unwrap_or_else(|| "None".to_string());
                let vars = format!("{:?}", vars);
                println!("WithDecl(vars: {}, expr: {})", vars, expr);
            }
            AST::Index(index) => {
                let index = format!("{:?}", index.0);
                println!("Index({})", index);
            }
        }
        Ok(())
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn ASTTrait<'a>) -> Result<()> {
        let mut tmp = ast.take();
        let res = self.inner_visit(exprs, &mut tmp);
        ast.replace(tmp);
        res
    }
}

pub fn debug_ast<'a>(ast: &mut AST<'a>, exprs: &mut Vec<Expr<'a>>) {
    let mut visitor = DebugASTVisitor::new();
    visitor.visit(exprs, ast).unwrap();
}
