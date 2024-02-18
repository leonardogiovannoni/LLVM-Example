mod ast;
mod ast_visitor;
mod debug_visitor;
mod decl_check;
mod expr;
mod lexer;
mod parser;
mod token;
use crate::ast::*;
use crate::ast_visitor::*;
use crate::debug_visitor::*;
use crate::decl_check::*;
use crate::expr::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;
use anyhow::bail;
use anyhow::Result;
use inkwell::context::Context;
use refslice::RefSlice;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
pub struct Sema;

impl Sema {
    pub fn semantic(&self, exprs: &State, ast: &Ast) -> Result<bool> {
        let check = DeclCheck::new();
        ast.accept(exprs, &check)?;
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

    pub fn compile(&self, exprs: &State, mut ast: Ast) -> Result<()> {
        let module = self.ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let mut to_ir = ToIRVisitor::new(self.ctx, Rc::clone(&module));
        to_ir.run(exprs, &mut ast)?;
        let s = module.print_to_string().to_string();
        println!("{}", s);
        Ok(())
    }
}

use dashmap::DashMap;


#[derive(Debug)]
pub struct ExprArena {
    exprs: DashMap<usize, RefCell<Expr>>,
    next_index: AtomicUsize,
}

impl ExprArena {
    pub fn new() -> Self {
        ExprArena {
            exprs: DashMap::new(),
            next_index: AtomicUsize::new(1),
        }
    }

    pub fn insert(&self, expr: Expr) -> ExprIndex {
        let rv = self.next_index.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let expr = RefCell::new(expr);
        self.exprs.insert(rv, expr);
        ExprIndex(rv)
    }

    pub fn get(&self, index: ExprIndex) -> Option<impl Deref<Target = RefCell<Expr>> + '_> {
        self.exprs.get(&index.0)
    }
}



#[derive(Debug)]
pub struct State {
    pub exprs: ExprArena,
}

fn routine() -> Result<()> {
    let input = std::env::args().nth(1).expect("no input");
    let input = input.chars().collect::<Vec<_>>();
    let input = Rc::from(input);
    let input = RefSlice::from(input);
    let lexer = Lexer::new(RefSlice::clone(&input));
    let mut parser = Parser::new(lexer, RefSlice::clone(&input));
    let state = State {
        exprs: ExprArena::new(),
    };
    let ast = parser.parse(&state);
    if parser.has_error {
        bail!("parse error");
    }
    let Some(ast) = ast else {
        bail!("parse error");
    };
    debug_ast(&ast, &state);
    let semantic = Sema;

    if semantic.semantic(&state, &ast)? {
        bail!("semantic error");
    }
    let ctx = Context::create();
    let codegen = CodeGen::new(&ctx);
    codegen.compile(&state, ast)
}

fn main() {
    if let Err(e) = routine() {
        eprintln!("error: {:?}", e);
    }
}
