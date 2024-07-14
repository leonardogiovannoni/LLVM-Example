mod ast;
mod ast_visitor;
mod codegen;
mod debug_visitor;
mod decl_check;
mod lexer;
mod parser;
mod token;
use crate::ast::*;
use crate::ast_visitor::*;
use crate::codegen::CodeGen;
use crate::codegen::Sema;
use crate::decl_check::*;
use crate::lexer::*;
use crate::parser::*;
use crate::token::*;
use anyhow::bail;
use anyhow::Result;
use debug_visitor::debug_ast;
use inkwell::context::Context;
use refslice::refstr::RefStr;

use std::rc::Rc;

fn run() -> Result<()> {
    let input = std::env::args().nth(1).expect("no input");
    println!("input: {}", input);
    let input = input.chars().collect::<String>();
    let input = RefStr::from(input);
    let lexer = Lexer::new(input.index(..));
    let mut parser = Parser::new(lexer, input.index(..));
    let ast = parser.parse();
    if parser.has_error {
        bail!("parse error");
    }
    let Ok(ast) = ast else {
        bail!("parse error");
    };

    //debug_ast(&ast, Rc::clone(&state));
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
