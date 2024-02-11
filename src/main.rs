mod ast;
mod ast_visitor;
mod debug_visitor;
mod expr;
mod lexer;
mod parser;
mod token;
use crate::ast::*;
use crate::ast_visitor::*;
use crate::debug_visitor::*;
use crate::expr::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;
use inkwell::context::Context;
use std::collections::HashSet;
use std::rc::Rc;

pub struct Sema;

impl Sema {
    pub fn semantic<'a>(&self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>) -> bool {
        let mut check = DeclCheck::new();
        ast.accept(exprs, &mut check).unwrap();
        check.has_error
    }
}

pub struct CodeGen<'a> {
    ctx: &'a Context,
}

impl<'a> CodeGen<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        CodeGen { ctx }
    }

    pub fn compile(&self, exprs: &mut Vec<Expr<'a>>, mut ast: AST<'a>) {
        let module = self.ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let mut to_ir = ToIRVisitor::new(self.ctx, Rc::clone(&module));
        to_ir.run(exprs, &mut ast).unwrap();
        module.print_to_stderr();
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let input = args.get(1).expect("no input file");
    let input = input.chars().collect::<Vec<_>>();
    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let mut exprs = Vec::new();
    let ast = parser.parse(&mut exprs);
    if parser.has_error {
        return;
    }
    let Some(mut ast) = ast else {
        return;
    };
    debug_ast(&mut ast, &mut exprs);
    let semantic = Sema;

    if semantic.semantic(&mut exprs, &mut ast) {
        println!("semantic error");
        return;
    }
    let ctx = Context::create();
    let codegen = CodeGen::new(&ctx);
    codegen.compile(&mut exprs, ast);
}
