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

    fn resolve_and_format_expr_index(&self, exprs: &[Expr<'a>], index: ExprIndex) -> String {
        let expr = &exprs[index.0];
        self.format_expr(exprs, expr)
    }

    fn format_expr(&self, exprs: &[Expr<'a>], expr: &Expr<'a>) -> String {
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
                let val = factor.val.iter().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                format!("Factor(kind: {}, val: {})", kind, val)
            }
        }
    }
}

impl<'a> AstVisitorTrait<'a> for DebugAstVisitor<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut Ast<'a>) -> Result<()> {
        match ast {
            Ast::BinaryOp(bin_op) => {
                let get_pretty_name = |idx: Option<ExprIndex>| {
                    idx.map(|idx| self.resolve_and_format_expr_index(exprs, idx))
                        .map(|expr| format!("Some({})", expr))
                        .unwrap_or_else(|| "None".to_string())
                };
                let lhs = get_pretty_name(bin_op.lhs_expr);
                let rhs = get_pretty_name(bin_op.rhs_expr);
                let op = format!("{:?}", bin_op.op);
                println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
            }
            Ast::Factor(factor) => {
                let val = factor.val.iter().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                println!("Factor(kind: {}, val: {})", kind, val);
            }
            Ast::WithDecl(with_decl) => {
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
            Ast::Index(index) => {
                let index = format!("{:?}", index.0);
                println!("Index({})", index);
            }
        }
        Ok(())
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn AstTrait<'a>) -> Result<()> {
        let mut tmp = ast.take();
        let res = self.inner_visit(exprs, &mut tmp);
        ast.replace(tmp);
        res
    }
}

pub fn debug_ast<'a>(ast: &mut Ast<'a>, exprs: &mut Vec<Expr<'a>>) {
    let mut visitor = DebugAstVisitor::new();
    visitor.visit(exprs, ast).unwrap();
}
