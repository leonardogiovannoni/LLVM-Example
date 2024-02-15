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
use std::cell::RefCell;
use std::collections::HashSet;
use std::ops::Range;
use std::rc::Rc;

pub struct Sema;

impl Sema {
    pub fn semantic<'a>(&self, exprs: &State, ast: &mut Ast) -> bool {
        let mut check = DeclCheck::new();
        ast.accept(exprs, &mut check).unwrap();
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

    pub fn compile(&self, exprs: &State, mut ast: Ast) {
        let module = self.ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let mut to_ir = ToIRVisitor::new(self.ctx, Rc::clone(&module));
        to_ir.run(exprs, &mut ast).unwrap();
        let s = module.print_to_string().to_string();
        println!("{}", s);
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span {
            start: range.start,
            end: range.end,
        }
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

pub struct State {
    pub exprs: RefCell<Vec<Expr>>,
}

fn main() {
    let input = std::env::args().nth(1).expect("no input");
    let input = input.chars().collect::<Vec<_>>();
    let input = Rc::from(input);
    let lexer = Lexer::new(Rc::clone(&input));
    let mut parser = Parser::new(lexer, Rc::clone(&input));
    let state = State {
        exprs: RefCell::new(Vec::new()),
    };
    let ast = parser.parse(&state);
    if parser.has_error {
        return;
    }
    let Some(mut ast) = ast else {
        return;
    };
    debug_ast(&ast, &state);
    let semantic = Sema;

    if semantic.semantic(&state, &mut ast) {
        println!("semantic error");
        return;
    }
    let ctx = Context::create();
    let codegen = CodeGen::new(&ctx);
    codegen.compile(&state, ast);
}
