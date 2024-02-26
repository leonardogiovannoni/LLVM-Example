use crate::*;
use anyhow::Result;
struct DebugAstVisitor<'a> {
    phantom: std::marker::PhantomData<&'a ()>,
    state: Rc<State>,
}

impl<'a> DebugAstVisitor<'a> {
    fn new(state: Rc<State>) -> DebugAstVisitor<'a> {
        DebugAstVisitor {
            phantom: std::marker::PhantomData,
            state,
        }
    }

    fn visit_binary_op(&self, bin_op: &BinaryOp) -> Result<()> {
        let get_pretty_name = |idx: usize| {
            let expr = self.state.exprs.get(idx).unwrap();
            self.format_expr(&expr)
        };
        let lhs = get_pretty_name(bin_op.lhs_expr);
        let rhs = get_pretty_name(bin_op.rhs_expr);
        let op = format!("{:?}", bin_op.op);
        println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
        Ok(())
    }

    fn visit_factor(&self, factor: &Factor) -> Result<()> {
        let kind = format!("{:?}", factor.kind);
        println!("Factor(kind: {}, val: {})", kind, factor.text.as_str());
        Ok(())
    }

    fn format_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::BinaryOp(bin_op) => {
                let lhs_expr = self.state.exprs.get(bin_op.lhs_expr).unwrap();
                let lhs = self.format_expr(&lhs_expr);
                let rhs_expr = self.state.exprs.get(bin_op.rhs_expr).unwrap();
                let rhs = self.format_expr(&rhs_expr);
                let op = format!("{:?}", bin_op.op);
                format!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op)
            }
            Expr::Factor(factor) => {
                let kind = format!("{:?}", factor.kind);
                format!("Factor(kind: {}, val: {})", kind, factor.text.as_str())
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

        let tmp = self.state.exprs.get(with_decl.expr).unwrap();
        let expr = self.format_expr(&tmp);
        let vars = format!("{:?}", vars);
        println!("WithDecl(vars: {}, expr: {})", vars, expr);
        Ok(())
    }

    fn visit_index(&self, index: usize) -> Result<()> {
        let index = self.state.exprs.get(index).unwrap();
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

pub fn debug_ast(ast: &Ast, state: Rc<State>) {
    let visitor = DebugAstVisitor::new(state);
    visitor.visit(ast).unwrap();
}
