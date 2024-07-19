use crate::util::RcStr;
use crate::util::Span;
use inkwell::{
    builder::Builder,
    module::{Linkage, Module},
    values::BasicValueEnum,
    AddressSpace,
};
use std::fmt::Debug;
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
};

use crate::*;
use anyhow::Result;

pub trait AstVisitorTrait<'a> {
    fn visit_with_decl(&self, ast: &WithDecl) -> Result<()>;
    fn visit_expr(&self, ast: &Expr) -> Result<()>;
    fn visit(&self, ast: &Ast) -> Result<()>;
}

#[derive(Debug)]
pub struct ToIRVisitor<'ctx> {
    context: &'ctx Context,
    module: Rc<Module<'ctx>>,
    builder: Builder<'ctx>,
    void_ty: inkwell::types::VoidType<'ctx>,
    int32_ty: inkwell::types::IntType<'ctx>,
    ptr_ty: inkwell::types::PointerType<'ctx>,
    int32_zero: inkwell::values::IntValue<'ctx>,
    v: Cell<BasicValueEnum<'ctx>>,
    name_map: RefCell<HashMap<RcStr, BasicValueEnum<'ctx>>>,
    text: Rc<str>,
}

impl<'ctx> ToIRVisitor<'ctx> {
    pub fn new(context: &'ctx Context, module: Rc<Module<'ctx>>, text: Rc<str>) -> Self {
        let builder = context.create_builder();
        let void_ty = context.void_type();
        let int32_ty = context.i32_type();
        let ptr_ty = int32_ty.ptr_type(AddressSpace::default());
        let int32_zero = int32_ty.const_int(0, true);

        Self {
            context,
            module,
            builder,
            void_ty,
            int32_ty,
            ptr_ty,
            int32_zero,
            v: Cell::new(int32_zero.into()),
            name_map: Default::default(),
            text,
        }
    }

    fn visit_binary_op(&self, bin_op: &BinaryOp) -> Result<()> {
        bin_op.lhs_expr.accept(self)?;
        let left = self.v.get();
        bin_op.rhs_expr.accept(self)?;
        let right = self.v.get();

        let op = match bin_op.op {
            Operator::Plus => Builder::build_int_nsw_add,
            Operator::Minus => Builder::build_int_nsw_sub,
            Operator::Mul => Builder::build_int_nsw_mul,
            Operator::Div => Builder::build_int_signed_div,
        };

        let v = op(
            &self.builder,
            left.into_int_value(),
            right.into_int_value(),
            "",
        )?
        .into();
        self.v.set(v);
        Ok(())
    }

    fn text(&self, span: Span) -> &str {
        &self.text[span.begin..span.end]
    }

    fn visit_factor(&self, factor: &Factor) -> Result<()> {
        match factor.kind {
            ValueKind::Ident => {
                let val = self.text(factor.span).to_owned();
                let span = Span {
                    begin: 0,
                    end: val.len(),
                };
                let val = RcStr {
                    string: val.into_boxed_str().into(),
                    span,
                };
                if let Some(&val) = self.name_map.borrow().get(&val) {
                    self.v.set(val);
                } else {
                    bail!("Variable \"{}\" not found", self.text(factor.span));
                }
            }
            _ => {
                let intval = self.text(factor.span).parse().expect("Invalid integer");
                let v = self.int32_ty.const_int(intval, true).into();
                self.v.set(v);
            }
        }
        Ok(())
    }

    pub fn run(&self, ast: &Ast) -> Result<()> {
        let main_fn_type = self
            .int32_ty
            .fn_type(&[self.int32_ty.into(), self.ptr_ty.into()], false);
        let main_fn = self
            .module
            .add_function("main", main_fn_type, Some(Linkage::External));
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        ast.accept(self)?;

        let calc_write_fn_type = self.void_ty.fn_type(&[self.int32_ty.into()], false);
        let calc_write_fn =
            self.module
                .add_function("calc_write", calc_write_fn_type, Some(Linkage::External));
        let v = self.v.get();
        self.builder
            .build_call(calc_write_fn, &[v.into()], "call_calc_write")?;

        self.builder.build_return(Some(&self.int32_zero))?;
        Ok(())
    }
}

impl<'a> AstVisitorTrait<'a> for ToIRVisitor<'a> {
    fn visit_with_decl(&self, with_decl: &WithDecl) -> Result<()> {
        let read_ftype = self.int32_ty.fn_type(&[self.ptr_ty.into()], false);
        let read_fn = self
            .module
            .add_function("calc_read", read_ftype, Some(Linkage::External));

        for &var in with_decl.vars.iter() {
            let str_val = self.context.const_string(self.text(var).as_bytes(), true);

            let global_str = self.module.add_global(
                str_val.get_type(),
                Some(AddressSpace::default()),
                &format!("{}.str", self.text(var)),
            );

            global_str.set_initializer(&str_val);
            global_str.set_linkage(Linkage::Private);
            global_str.set_constant(true);

            let call =
                self.builder
                    .build_call(read_fn, &[global_str.as_pointer_value().into()], "")?;

            let left = call
                .try_as_basic_value()
                .left()
                .ok_or(anyhow::anyhow!("not a basic value"))?;
            let rc_str = RcStr {
                string: Rc::clone(&self.text),
                span: var,
            };
            self.name_map.borrow_mut().insert(rc_str, left);
        }
        with_decl.expr.accept(self)?;
        Ok(())
    }

    fn visit_expr(&self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::BinaryOp(bin_op) => self.visit_binary_op(bin_op),
            Expr::Factor(factor) => self.visit_factor(factor),
        }
    }

    fn visit(&self, ast: &Ast) -> Result<()> {
        match ast {
            Ast::WithDecl(with_decl) => self.visit_with_decl(with_decl),
            Ast::Expr(expr) => self.visit_expr(expr),
        }
    }
}
