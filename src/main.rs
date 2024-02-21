#![feature(lazy_cell)]
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
use refslice::refstr::RefStr;

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
pub struct Sema;

impl Sema {
    pub fn semantic(&self, ast: &Ast, state: Rc<State>) -> Result<bool> {
        let check = DeclCheck::new(state);
        ast.accept(&check)?;
        Ok(check.has_error.get())
    }
}

#[derive(Debug)]
pub struct Arena<T> {
    data: RefCell<HashMap<usize, Rc<T>>>,
    next_id: Cell<usize>,
}

impl<T: Default> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Arena<T> {
    pub fn new() -> Arena<T> {
        Arena {
            data: Default::default(),
            next_id: Cell::new(1),
        }
    }

    pub fn insert(&self, value: T) -> usize {
        let id = self.next_id.get();
        self.next_id.set(id + 1);
        self.data.borrow_mut().insert(id, Rc::new(value));
        id
    }

    #[inline(always)]
    pub fn get(&self, id: usize) -> Option<impl std::ops::Deref<Target = T> + '_> {
        self.data.borrow().get(&id).cloned()
    }
}

#[derive(Debug)]
pub struct State {
    exprs: Arena<Expr>,
}

pub struct CodeGen<'a> {
    ctx: &'a Context,
}

impl<'a> CodeGen<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        CodeGen { ctx }
    }

    pub fn compile(&self, ast: Ast, state: Rc<State>) -> Result<()> {
        let module = self.ctx.create_module("calc.expr");
        let module = Rc::new(module);
        let to_ir = ToIRVisitor::new(self.ctx, Rc::clone(&module), state);
        to_ir.run(&ast)?;
        let s = module.print_to_string().to_string();
        println!("{}", s);
        Ok(())
    }
}

fn run() -> Result<()> {
    let input = std::env::args().nth(1).expect("no input");
    let input = input.chars().collect::<String>().into_boxed_str();
    let input = RefStr::from(input.to_string());
    let lexer = Lexer::new(input.index(..));
    let state = Rc::new(State {
        exprs: Arena::new(),
    });
    let mut parser = Parser::new(lexer, input.index(..), Rc::clone(&state));
    let ast = parser.parse();
    if parser.has_error {
        bail!("parse error");
    }
    let Some(ast) = ast else {
        bail!("parse error");
    };

    debug_ast(&ast, Rc::clone(&state));
    let semantic = Sema;

    if semantic.semantic(&ast, Rc::clone(&state))? {
        bail!("semantic error");
    }
    let ctx = Context::create();
    let codegen = CodeGen::new(&ctx);
    codegen.compile(ast, state)
}

fn main() {
    if let Err(e) = run() {
        eprintln!("error: {:?}", e);
    }
}
