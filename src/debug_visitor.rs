use crate::*;
use anyhow::Result;
use util::Span;
struct DebugAstVisitor<'a> {
    phantom: std::marker::PhantomData<&'a ()>,
    text: Rc<str>,
}

impl<'a> DebugAstVisitor<'a> {
    fn new(text: Rc<str>) -> DebugAstVisitor<'a> {
        DebugAstVisitor {
            phantom: std::marker::PhantomData,
            text,
        }
    }

    fn text(&self, span: Span) -> &str {
        &self.text[span.begin..span.end]
    }

    fn visit_binary_op(&self, bin_op: &BinaryOp) -> Result<()> {
        let [lhs, rhs] = [&bin_op.lhs_expr, &bin_op.rhs_expr].map(|expr| self.format_expr(expr));
        let op = format!("{:?}", bin_op.op);
        println!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op);
        Ok(())
    }

    fn visit_factor(&self, factor: &Factor) -> Result<()> {
        let kind = format!("{:?}", factor.kind);
        println!("Factor(kind: {}, val: {:?})", kind, self.text(factor.span));
        Ok(())
    }

    fn format_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::BinaryOp(bin_op) => {
                let lhs = self.format_expr(&bin_op.lhs_expr);
                let rhs = self.format_expr(&bin_op.rhs_expr);
                let op = format!("{:?}", bin_op.op);
                format!("BinaryOp(lhs: {}, rhs: {}, op: {})", lhs, rhs, op)
            }
            Expr::Factor(factor) => {
                let kind = format!("{:?}", factor.kind);
                format!("Factor(kind: {}, val: {:?})", kind, self.text(factor.span))
            }
        }
    }
}

impl<'a> AstVisitorTrait<'a> for DebugAstVisitor<'a> {
    fn visit_with_decl(&self, with_decl: &WithDecl) -> Result<()> {
        let vars = with_decl
            .vars_iter(&*self.text)
            .map(|v| v.to_owned())
            .collect::<Vec<_>>();

        let tmp = &with_decl.expr;
        let expr = self.format_expr(tmp);
        let vars = format!("{:?}", vars);
        println!("WithDecl(vars: {}, expr: {})", vars, expr);
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::BinaryOp(bin_op) => self.visit_binary_op(bin_op),
            Expr::Factor(factor) => self.visit_factor(factor),
        }
    }

    fn visit(&self, ast: &Ast) -> Result<()> {
        match ast {
            Ast::WithDecl(with_decl) => self.visit_with_decl(with_decl),
            Ast::Expr(expr) => self.visit_expr(expr),
        }
    }
}

pub fn debug_ast(ast: &Ast, text: Rc<str>) {
    let visitor = DebugAstVisitor::new(text);
    visitor.visit(ast).unwrap();
}
