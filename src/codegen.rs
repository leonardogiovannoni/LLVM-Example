use std::rc::Rc;

use anyhow::Result;
use inkwell::context::Context;

use crate::{Ast, AstTrait, DeclCheck, ToIRVisitor};

pub struct Sema;

impl Sema {
    pub fn semantic(&self, ast: &Ast) -> Result<bool> {
        let check = DeclCheck::new();
        ast.accept(&check)?;
        Ok(check.has_error.get())
    }
}

pub struct CodeGen<'a> {
    ctx: &'a Context,
}

impl<'a> CodeGen<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        CodeGen { ctx }
    }

    pub fn compile(&self, ast: Ast) -> Result<()> {
        let module = self.ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let to_ir = ToIRVisitor::new(self.ctx, Rc::clone(&module));
        to_ir.run(&ast)?;
        let s = module.print_to_string().to_string();
        println!("{}", s);
        Ok(())
    }
}
