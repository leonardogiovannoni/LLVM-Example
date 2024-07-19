mod ast;
mod ast_visitor;
mod codegen;
mod debug_visitor;
mod decl_check;
mod lexer;
mod parser;
mod token;
mod util;
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
use std::fmt::Debug;
use std::rc::Rc;

fn run(mut args: impl Iterator<Item = String>) -> Result<String> {
    let input = args.nth(0).expect("no input");
    let input: Rc<str> = input.into_boxed_str().into();
    let lexer = Lexer::new(Rc::clone(&input));
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;

    debug_ast(&ast, Rc::clone(&input));
    let semantic = Sema::new(Rc::clone(&input));
    if semantic.semantic(&ast)? {
        bail!("semantic error");
    }
    let ctx = Context::create();
    let codegen = CodeGen::new(&ctx, input);
    codegen.compile(ast)
}

fn main() {
    match run(std::env::args().skip(1)) {
        Ok(s) => {
            println!("{}", s);
        }
        Err(e) => {
            eprintln!("error: {:?}", e);
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = "with a, b, c, d, e, f, g: (a*a*a + b*b - c*d + e*e - a*b + c*c*d - 5*e*a + d*d*d) * (2*f - 10*g)".to_string();
        let expected_output = r#"
            ; ModuleID = 'calc.expr'
            source_filename = "calc.expr"

            @a.str = private constant [2 x i8] c"a\00"
            @b.str = private constant [2 x i8] c"b\00"
            @c.str = private constant [2 x i8] c"c\00"
            @d.str = private constant [2 x i8] c"d\00"
            @e.str = private constant [2 x i8] c"e\00"
            @f.str = private constant [2 x i8] c"f\00"
            @g.str = private constant [2 x i8] c"g\00"

            define i32 @main(i32 %0, ptr %1) {
            entry:
              %2 = call i32 @calc_read(ptr @a.str)
              %3 = call i32 @calc_read(ptr @b.str)
              %4 = call i32 @calc_read(ptr @c.str)
              %5 = call i32 @calc_read(ptr @d.str)
              %6 = call i32 @calc_read(ptr @e.str)
              %7 = call i32 @calc_read(ptr @f.str)
              %8 = call i32 @calc_read(ptr @g.str)
              %9 = mul nsw i32 %2, %2
              %10 = mul nsw i32 %9, %2
              %11 = mul nsw i32 %3, %3
              %12 = add nsw i32 %10, %11
              %13 = mul nsw i32 %4, %5
              %14 = sub nsw i32 %12, %13
              %15 = mul nsw i32 %6, %6
              %16 = add nsw i32 %14, %15
              %17 = mul nsw i32 %2, %3
              %18 = sub nsw i32 %16, %17
              %19 = mul nsw i32 %4, %4
              %20 = mul nsw i32 %19, %5
              %21 = add nsw i32 %18, %20
              %22 = mul nsw i32 5, %6
              %23 = mul nsw i32 %22, %2
              %24 = sub nsw i32 %21, %23
              %25 = mul nsw i32 %5, %5
              %26 = mul nsw i32 %25, %5
              %27 = add nsw i32 %24, %26
              %28 = mul nsw i32 2, %7
              %29 = mul nsw i32 10, %8
              %30 = sub nsw i32 %28, %29
              %31 = mul nsw i32 %27, %30
              call void @calc_write(i32 %31)
              ret i32 0
            }

            declare i32 @calc_read(ptr)

            declare void @calc_write(i32)
        "#;

        let tmp = run([input].into_iter());
        match tmp {
            Ok(s) => {
                let s = s.trim();
                let expected_output = expected_output.trim();
                let s = s
                    .split_whitespace()
                    .map(|x| x.trim())
                    .filter(|x| !x.is_empty())
                    .collect::<String>()
                    .replace("\n", "");
                let expected_output = expected_output
                    .split_whitespace()
                    .map(|x| x.trim())
                    .filter(|x| !x.is_empty())
                    .collect::<String>()
                    .replace("\n", "");
                assert_eq!(s, expected_output);
            }
            Err(e) => {
                panic!("error: {:?}", e);
            }
        }
    }
}
