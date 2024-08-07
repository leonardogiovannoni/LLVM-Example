use std::rc::Rc;

use anyhow::{bail, Result};
use inkwell::context::Context;

use crate::{Ast, AstTrait, DeclCheck, ToIRVisitor};

pub struct Sema<'a> {
    text: &'a str,
}

impl<'a> Sema<'a> {
    pub fn new(text: &'a str) -> Self {
        Sema { text }
    }
    pub fn semantic(&self, ast: &Ast) -> Result<()> {
        let check = DeclCheck::new(self.text);
        ast.accept(&check)?;
        if check.has_error.get() {
            bail!("Declaration error")
        } else {
            Ok(())
        }
    }
}

pub struct CodeGen<'a> {
    ctx: Context,
    text: &'a str,
}

impl<'a> CodeGen<'a> {
    pub fn new(text: &'a str) -> Self {
        CodeGen {
            ctx: Context::create(),
            text,
        }
    }

    pub fn compile(&self, ast: Ast) -> Result<String> {
        let module = self.ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let to_ir = ToIRVisitor::new(&self.ctx, Rc::clone(&module), self.text);
        to_ir.run(&ast)?;
        Ok(module.print_to_string().to_string())
    }
}
