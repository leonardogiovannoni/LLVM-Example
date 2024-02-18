mod ast;
mod ast_visitor;
mod debug_visitor;
mod decl_check;
mod lexer;
mod parser;
mod token;
use crate::ast::*;
use crate::ast_visitor::*;
use crate::debug_visitor::*;
use crate::decl_check::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;
use anyhow::bail;
use anyhow::Result;
use inkwell::context::Context;
use refslice::RefSlice;
use std::rc::Rc;
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

    pub fn compile(&self, mut ast: Ast) -> Result<()> {
        let module = self.ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let mut to_ir = ToIRVisitor::new(self.ctx, Rc::clone(&module));
        to_ir.run(&mut ast)?;
        let s = module.print_to_string().to_string();
        println!("{}", s);
        Ok(())
    }
}

fn run() -> Result<()> {
    let input = std::env::args().nth(1).expect("no input");
    let input = input.chars().collect::<Vec<_>>();
    let input = RefSlice::from(Rc::from(input));
    let lexer = Lexer::new(RefSlice::clone(&input));
    let mut parser = Parser::new(lexer, input.index(..));
    let ast = parser.parse();
    if parser.has_error {
        bail!("parse error");
    }
    let Some(ast) = ast else {
        bail!("parse error");
    };
    debug_ast(&ast);
    let semantic = Sema;

    if semantic.semantic(&ast)? {
        bail!("semantic error");
    }
    let ctx = Context::create();
    let codegen = CodeGen::new(&ctx);
    codegen.compile(ast)
}

fn main() {
    if let Err(e) = run() {
        eprintln!("error: {:?}", e);
    }
}
