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
use crossbeam_skiplist::map::Entry;
use crossbeam_skiplist::SkipMap;
use inkwell::context::Context;
use refslice::refstr::RefStr;

use std::cell::Cell;
use std::cell::LazyCell;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::LazyLock;
pub struct Sema;

impl Sema {
    pub fn semantic(&self, ast: &Ast) -> Result<bool> {
        let check = DeclCheck::new();
        ast.accept(&check)?;
        Ok(check.has_error.get())
    }
}



pub struct Arena<T> {
    data: SkipMap<usize, T>,
    next_id: Cell<usize>,
}

impl<T: Send + 'static> Arena<T> {
    pub fn new() -> Arena<T> {
        Arena {
            data: Default::default(),
            next_id: Cell::new(1),
        }
    }

    pub fn insert(&self, value: T) -> usize {
        let id = self.next_id.get();
        self.next_id.set(id + 1);
        self.data.insert(id, value);
        id
    }

    pub fn get(&self, id: usize) -> Option<impl std::ops::Deref<Target = T> + '_> {
        struct Dummy<'a, K, V> (Entry<'a, K, V>);
        impl<'a, K, V> Deref for Dummy<'a, K, V> {
            type Target = V;
            fn deref(&self) -> &V {
                self.0.value()
            }
        }
        self.data.get(&id).map(|entry| Dummy(entry))
    }
}

unsafe impl<T> Send for Arena<T> {}
unsafe impl<T> Sync for Arena<T> {}


pub static EXPR: LazyLock<Arena<Expr>> = LazyLock::new(|| Arena::new());

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
    let input = std::env::args().skip(1).next().expect("no input");
    let input = input.chars().collect::<String>().into_boxed_str();
    let input = RefStr::from(input.to_string());
    let lexer = Lexer::new(input.index(..));
    let mut parser = Parser::new(lexer, input.index(..));
    let ast = parser.parse();
    if parser.has_error {
        bail!("parse error");
    }
    let Some(ast) = ast else {
        bail!("parse error");
    };
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
