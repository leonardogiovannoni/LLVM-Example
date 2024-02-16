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

    fn resolve_and_format_expr_index(&self, exprs: &[Expr], index: ExprIndex) -> String {
        let expr = &exprs[index.0];
        self.format_expr(exprs, expr)
    }

    fn format_expr(&self, exprs: &[Expr], expr: &Expr) -> String {
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
                /*  let val = factor.text[factor.span.start..factor.span.end]
                .iter()
                .collect::<String>();*/
                let val = factor.text.iter().collect::<String>();
                let kind = format!("{:?}", factor.kind);
                format!("Factor(kind: {}, val: {})", kind, val)
            }
        }
    }
}

impl<'a> AstVisitorTrait<'a> for DebugAstVisitor<'a> {
    fn visit(&self, exprs: &State, ast: &impl AstTrait) -> Result<()> {
        let bin_op_fn = |bin_op: &BinaryOp| {
            let exprs = &exprs.exprs;
            let get_pretty_name = |idx: Option<ExprIndex>| {
                idx.map(|idx| self.resolve_and_format_expr_index(&exprs.borrow(), idx))
                    .map(|expr| format!("Some({})", expr))
                    .unwrap_or_else(|| "None".to_string())
            };
            let lhs = get_pretty_name(bin_op.lhs_expr);
            let rhs = get_pretty_name(bin_op.rhs_expr);
            let op = format!("{:?}", bin_op.op);
            println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
            Ok(())
        };

        let factor_fn = |factor: &Factor| {
            /*let val = factor.text[factor.span.start..factor.span.end]
            .iter()
            .collect::<String>();*/
            let val = factor.text.iter().collect::<String>();
            let kind = format!("{:?}", factor.kind);
            println!("Factor(kind: {}, val: {})", kind, val);
            Ok(())
        };

        let with_decl_fn = |with_decl: &WithDecl| {
            let vars = with_decl
                .vars_iter()
                .map(|v| v.iter().collect::<String>())
                .collect::<Vec<_>>();
            let exprs = &exprs.exprs;
            let expr = with_decl
                .expr_index
                .map(|idx| self.resolve_and_format_expr_index(&exprs.borrow(), idx));
            let expr = expr
                .map(|expr| format!("Some({})", expr))
                .unwrap_or_else(|| "None".to_string());
            let vars = format!("{:?}", vars);
            println!("WithDecl(vars: {}, expr: {})", vars, expr);
            Ok(())
        };

        let index_fn = |index: &ExprIndex| {
            let index = format!("{:?}", index.0);
            println!("Index({})", index);
            Ok(())
        };

        ast.callbacks(
            Some(bin_op_fn),
            Some(factor_fn),
            Some(with_decl_fn),
            Some(index_fn),
        )?;
        Ok(())
    }
}

pub fn debug_ast(ast: &Ast, state: &State) {
    let visitor = DebugAstVisitor::new();
    visitor.visit(state, ast).unwrap();
}
