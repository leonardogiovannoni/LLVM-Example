mod ast;
mod ast_visitor;
mod expr;
mod lexer;
mod parser;
mod token;

use crate::ast::*;
use crate::ast_visitor::*;
use crate::expr::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;
use inkwell::context::Context;
use std::collections::HashSet;

pub struct Sema;

impl Sema {
    pub fn semantic<'a>(&self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>) -> bool {
        let check = DeclCheck::new();
        let mut check = ASTVisitor::DeclCheck(check);
        ast.accept(exprs, &mut check);
        true
    }
}

pub struct CodeGen;

impl CodeGen {
    pub fn compile<'a>(&self, _exprs: &mut Vec<Expr<'a>>, _ast: AST<'a>) {
        let ctx = Context::create();
        let _i32_type = ctx.i32_type();
        let _module = ctx.create_module("calc.expr");
        todo!();
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
    if !semantic.semantic(&mut exprs, &mut ast) {}
}
