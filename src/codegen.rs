use std::rc::Rc;

use inkwell::context::Context;

use crate::{Ast, AstTrait, DeclCheck, State, ToIRVisitor};

pub struct Sema;

impl Sema {
    pub fn semantic(&self, exprs: &State, ast: &Ast) -> bool {
        let check = DeclCheck::new();
        ast.accept(exprs, &check).unwrap();
        check.has_error.get()
    }
}

pub struct CodeGen<'a> {
    ctx: &'a Context,
}

impl<'a> CodeGen<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        CodeGen { ctx }
    }

    pub fn compile(&self, exprs: &State, ast: Ast) {
        let module = self.ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let to_ir = ToIRVisitor::new(self.ctx, Rc::clone(&module));
        to_ir.run(exprs, &mut ast).unwrap();
        let s = module.print_to_string().to_string();
        println!("{}", s);
    }
}
