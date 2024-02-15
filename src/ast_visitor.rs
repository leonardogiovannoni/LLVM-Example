use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
};

use enum_dispatch::enum_dispatch;
use inkwell::{
    builder::Builder,
    module::{Linkage, Module},
    values::BasicValueEnum,
    AddressSpace,
};

use crate::*;
use anyhow::{bail, Result};

#[derive(Debug)]
#[enum_dispatch]
pub enum AstVisitor<'a> {
    DeclCheck(DeclCheck),
    ToIRVisitor(ToIRVisitor<'a>),
}

impl<'a> Default for AstVisitor<'a> {
    fn default() -> Self {
        AstVisitor::DeclCheck(DeclCheck::new())
    }
}

#[enum_dispatch(AstVisitor)]
pub trait AstVisitorTrait<'a> {
    //fn inner_visit(&self, exprs: &mut Vec<Expr>, ast: &mut Ast) -> Result<()>;
    fn visit(&self, exprs: &State, ast: &impl AstTrait) -> Result<()>;
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
    v: RefCell<BasicValueEnum<'ctx>>,
    name_map: RefCell<HashMap<String, BasicValueEnum<'ctx>>>,
}

impl<'ctx> ToIRVisitor<'ctx> {
    pub fn new(context: &'ctx Context, module: Rc<Module<'ctx>>) -> Self {
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
            v: RefCell::new(int32_zero.into()),
            name_map: Default::default(),
        }
    }
    pub fn run(&mut self, state: &State, tree: &mut Ast) -> Result<()> {
        let main_fn_type = self
            .int32_ty
            .fn_type(&[self.int32_ty.into(), self.ptr_ty.into()], false);
        let main_fn = self
            .module
            .add_function("main", main_fn_type, Some(Linkage::External));
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        tree.accept(state, self)?;

        let calc_write_fn_type = self.void_ty.fn_type(&[self.int32_ty.into()], false);
        let calc_write_fn =
            self.module
                .add_function("calc_write", calc_write_fn_type, Some(Linkage::External));
        let v = *self.v.borrow();
        self.builder
            .build_call(calc_write_fn, &[v.into()], "call_calc_write")?;

        self.builder.build_return(Some(&self.int32_zero))?;
        Ok(())
    }
}

impl<'a> Default for ToIRVisitor<'a> {
    fn default() -> Self {
        todo!()
    }
}

impl<'a> AstVisitorTrait<'a> for ToIRVisitor<'a> {
    fn visit(&self, state: &State, ast: &impl AstTrait) -> Result<()> {
        let bin_op_fn = |bin_op: &BinaryOp| {
            let lhs = bin_op.lhs_expr.unwrap();
            let rhs = bin_op.rhs_expr.unwrap();
            lhs.accept(state, self)?;
            let left = *self.v.borrow();
            rhs.accept(state, self)?;
            let right = *self.v.borrow();

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
            *self.v.borrow_mut() = v;
            Ok(())
        };

        let factor_fn = |factor: &Factor| {
            match factor.kind {
                ValueKind::Ident => {
                    let val = factor.text[factor.span.start..factor.span.end]
                        .iter()
                        .collect::<String>();
                    if let Some(&val) = self.name_map.borrow().get(&val) {
                        *self.v.borrow_mut() = val;
                    } else {
                        panic!("Variable not found");
                    }
                }
                _ => {
                    let val = factor.text[factor.span.start..factor.span.end]
                        .iter()
                        .collect::<String>();
                    let intval = val.parse::<i64>().expect("Invalid integer");
                    let v = self.int32_ty.const_int(intval as u64, true).into();
                    *self.v.borrow_mut() = v;
                }
            }
            Ok(())
        };

        let with_decl_fn = |with_decl: &WithDecl| {
            let read_ftype = self.int32_ty.fn_type(&[self.ptr_ty.into()], false);
            let read_fn =
                self.module
                    .add_function("calc_read", read_ftype, Some(Linkage::External));

            for var in with_decl.vars_iter() {
                let var = var.iter().collect::<String>();
                let str_val = self.context.const_string(var.as_bytes(), true);

                let global_str = self.module.add_global(
                    str_val.get_type(),
                    Some(AddressSpace::default()),
                    &format!("{}.str", var),
                );

                global_str.set_initializer(&str_val);
                global_str.set_linkage(Linkage::Private);
                global_str.set_constant(true);

                let call = self.builder.build_call(
                    read_fn,
                    &[global_str.as_pointer_value().into()],
                    "",
                )?;
                self.name_map
                    .borrow_mut()
                    .insert(var, call.try_as_basic_value().left().unwrap());
            }
            with_decl.expr_index.unwrap().accept(state, self)?;
            Ok(())
        };

        let index_fn = |index: &ExprIndex| {
            let exprs = &state.exprs;
            let tmp = exprs.borrow();
            let e = tmp.get(index.0).unwrap();
            let res = e.accept(state, self);
            if res.is_err() {
                return res;
            }
            Ok(())
        };

        ast.callbacks(
            Some(bin_op_fn),
            Some(factor_fn),
            Some(with_decl_fn),
            Some(index_fn),
        )?;
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct DeclCheck {
    pub scope: RefCell<HashSet<Span>>,
    pub has_error: Cell<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorType {
    Twice,
    Not,
}

impl DeclCheck {
    pub fn new() -> Self {
        DeclCheck {
            scope: RefCell::new(HashSet::new()),
            has_error: Cell::new(false),
        }
    }

    pub fn error(&self, err: ErrorType, s: Span) {
        let tmp = if err == ErrorType::Twice {
            "twice"
        } else {
            "not"
        };
        println!("Variable {:?} is {} declared", s, tmp);
        self.has_error.set(true);
    }
}

impl<'a> AstVisitorTrait<'a> for DeclCheck {
    fn visit(&self, state: &State, ast: &impl AstTrait) -> Result<()> {
        let bin_op_fn = |bin_op: &BinaryOp| {
            let lhs = bin_op.lhs_expr.unwrap();
            let rhs = bin_op.rhs_expr.unwrap();
            lhs.accept(state, self)?;
            rhs.accept(state, self)?;
            Ok(())
        };

        let factor_fn = |factor: &Factor| {
            if factor.kind == ValueKind::Ident && !self.scope.borrow().contains(&factor.span) {
                self.error(ErrorType::Not, factor.span);
            }
            Ok(())
        };

        let with_decl_fn = |with_decl: &WithDecl| {
            for i in with_decl.vars.iter() {
                if self.scope.borrow().contains(i) {
                    self.error(ErrorType::Twice, *i);
                    bail!("Variable declared twice");
                }
                self.scope.borrow_mut().insert(*i);
            }
            Ok(())
        };

        let index_fn = |index: &ExprIndex| {
            let exprs = &state.exprs;
            let tmp = exprs.borrow();
            let e = tmp.get(index.0).unwrap();
            let res = e.accept(state, self);
            if res.is_err() {
                return res;
            }
            Ok(())
        };

        ast.callbacks(
            Some(bin_op_fn),
            Some(factor_fn),
            Some(with_decl_fn),
            Some(index_fn),
        )?;
        Ok(())
    }
}
