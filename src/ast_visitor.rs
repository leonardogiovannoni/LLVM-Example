use std::{any::Any, cell::{Cell, RefCell}, collections::HashMap};

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
    fn visit(&self, exprs: &mut Vec<Expr>, ast: &impl AstTrait) -> Result<()>;
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
    pub fn run(&mut self, exprs: &mut Vec<Expr>, tree: &mut Ast) -> Result<()> {
        let main_fn_type = self
            .int32_ty
            .fn_type(&[self.int32_ty.into(), self.ptr_ty.into()], false);
        let main_fn = self
            .module
            .add_function("main", main_fn_type, Some(Linkage::External));
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        tree.accept(exprs, self)?;

        let calc_write_fn_type = self.void_ty.fn_type(&[self.int32_ty.into()], false);
        let calc_write_fn =
            self.module
                .add_function("calc_write", calc_write_fn_type, Some(Linkage::External));
        let v= *self.v.borrow();
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
   /*fn inner_visit(&self, exprs: &mut Vec<Expr>, ast: &mut Ast) -> Result<()> {
        match ast {
            Ast::BinaryOp(node) => {
                node.lhs_expr.unwrap().accept(exprs, self)?;
                let left = *self.v.borrow();
                node.rhs_expr.unwrap().accept(exprs, self)?;
                let right = *self.v.borrow();

                let op = match node.op {
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
            }
            Ast::Factor(factor) => match factor.kind {
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
            },
            Ast::WithDecl(node) => {
                let read_ftype = self.int32_ty.fn_type(&[self.ptr_ty.into()], false);
                let read_fn =
                    self.module
                        .add_function("calc_read", read_ftype, Some(Linkage::External));

                for var in node.vars_iter() {
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
                    self.name_map.borrow_mut()
                        .insert(var, call.try_as_basic_value().left().unwrap());
                }
                node.expr_index.unwrap().accept(exprs, self)?;
            }
            Ast::Index(expr_index) => {
                let e = exprs.get_mut(expr_index.0).unwrap();
                let mut e = std::mem::take(e);
                let res = e.accept(exprs, self);
                exprs[expr_index.0] = e;
                if res.is_err() {
                    return res;
                }
            }
        }
        Ok(())
    }*/

    fn visit(&self, exprs: &mut Vec<Expr>, ast: &impl AstTrait) -> Result<()> {
        /*let mut tmp = ast.take();
        let res = self.inner_visit(exprs, &mut tmp);
        ast.replace(tmp);
        res*/
        let exprs2 = exprs as *mut Vec<Expr>;

        let bin_op_fn = |bin_op: &BinaryOp| {
            let mut lhs = bin_op.lhs_expr.unwrap();
            let mut rhs = bin_op.rhs_expr.unwrap();
            lhs.accept(exprs, self)?;
            let left = *self.v.borrow();
            rhs.accept(exprs, self)?;
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
            let exprs = unsafe { &mut *exprs2 };
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
                self.name_map.borrow_mut()
                    .insert(var, call.try_as_basic_value().left().unwrap());
            }
            with_decl.expr_index.unwrap().accept(exprs, self)?;
            Ok(())
        };

        let index_fn = |index: &ExprIndex| {
            let exprs = unsafe { &mut *exprs2 };
            let e = exprs.get_mut(index.0).unwrap();
            let mut e = std::mem::take(e);
            let res = e.accept(exprs, self);
            exprs[index.0] = e;
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
    //pub scope: HashSet<&'a [char]>,
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
    /*fn inner_visit(&self, exprs: &mut Vec<Expr>, node: &mut Ast) -> Result<()> {
        match node {
            Ast::BinaryOp(node) => {
                // TODO: check for existence of ExprIndex in exprs
                for scan in [node.lhs_expr, node.rhs_expr].iter_mut() {
                    if let Some(mut expr) = scan {
                        expr.accept(exprs, self)?;
                    } else {
                        self.has_error.set(true);
                    }
                }
            }
            Ast::Factor(node) => {
                if node.kind == ValueKind::Ident && !self.scope.borrow().contains(&node.span) {
                    self.error(ErrorType::Not, node.span);
                }
            }
            Ast::WithDecl(node) => {
                for i in node.vars.iter() {
                    if self.scope.borrow().contains(i) {
                        self.error(ErrorType::Twice, *i);
                        bail!("Variable declared twice");
                    }
                    self.scope.borrow_mut().insert(*i);
                }

                if let Some(mut expr) = node.expr_index {
                    expr.accept(exprs, self)?;
                } else {
                    self.has_error.set(true);
                }
            }
            Ast::Index(_) => {}
        }
        Ok(())
    }*/

    fn visit(&self, exprs: &mut Vec<Expr>, ast: &impl AstTrait) -> Result<()> {
        let exprs2 = exprs as *mut Vec<Expr>;
        let bin_op_fn = |bin_op: &BinaryOp| {
            let mut lhs = bin_op.lhs_expr.unwrap();
            let mut rhs = bin_op.rhs_expr.unwrap();
            lhs.accept(exprs, self)?;
            rhs.accept(exprs, self)?;
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
            let exprs = unsafe { &mut *exprs2 };
            let e = exprs.get_mut(index.0).unwrap();
            let mut e = std::mem::take(e);
            let res = e.accept(exprs, self);
            exprs[index.0] = e;
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
