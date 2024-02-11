mod ast;
mod ast_visitor;
mod expr;
mod lexer;
mod parser;
mod token;
mod debug_visitor;
use crate::ast::*;
use crate::ast_visitor::*;
use crate::expr::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;
use crate::debug_visitor::*;
use inkwell::context::Context;
use std::collections::HashSet;
use std::rc::Rc;

pub struct Sema;

impl Sema {
    pub fn semantic<'a>(&self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>) -> bool {
        let mut check = DeclCheck::new();
        ast.accept(exprs, &mut check);
        check.has_error
    }
}

pub struct CodeGen;

impl CodeGen {
    pub fn compile<'a>(&self, exprs: &mut Vec<Expr<'a>>, mut ast: AST<'a>) {
        let ctx = Box::leak(Box::new(Context::create()));
        let module = ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let mut to_ir = ToIRVisitor::new(ctx, Rc::clone(&module));
        to_ir.run(exprs, &mut ast);
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
    //debug_ast(&mut ast, &mut exprs);
    let semantic = Sema;
    if semantic.semantic(&mut exprs, &mut ast) {
        println!("semantic error");
        return;
    }
    let codegen = CodeGen;
    codegen.compile(&mut exprs, ast);
}
