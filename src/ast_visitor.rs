use std::collections::HashMap;

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
    DeclCheck(DeclCheck<'a>),
    ToIRVisitor(ToIRVisitor<'a>),
}

impl<'a> Default for AstVisitor<'a> {
    fn default() -> Self {
        AstVisitor::DeclCheck(DeclCheck::new())
    }
}

#[enum_dispatch(AstVisitor)]
pub trait AstVisitorTrait<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut Ast<'a>) -> Result<()>;
    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn AstTrait<'a>) -> Result<()>;
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
    v: BasicValueEnum<'ctx>,
    name_map: HashMap<String, BasicValueEnum<'ctx>>,
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
            v: int32_zero.into(),
            name_map: Default::default(),
        }
    }
    pub fn run(&mut self, exprs: &mut Vec<Expr<'ctx>>, tree: &mut Ast<'ctx>) -> Result<()> {
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
        self.builder
            .build_call(calc_write_fn, &[self.v.into()], "call_calc_write")?;

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
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut Ast<'a>) -> Result<()> {
        match ast {
            Ast::BinaryOp(node) => {
                node.lhs_expr
                    .ok_or(anyhow::anyhow!("lhs_expr is None"))?
                    .accept(exprs, self)?;
                let left = self.v;
                node.rhs_expr
                    .ok_or(anyhow::anyhow!("rhs_expr is None"))?
                    .accept(exprs, self)?;
                let right = self.v;

                let op = match node.op {
                    Operator::Plus => Builder::build_int_nsw_add,
                    Operator::Minus => Builder::build_int_nsw_sub,
                    Operator::Mul => Builder::build_int_nsw_mul,
                    Operator::Div => Builder::build_int_signed_div,
                };
                self.v = op(
                    &self.builder,
                    left.into_int_value(),
                    right.into_int_value(),
                    "",
                )?
                .into();
            }
            Ast::Factor(factor) => match factor.kind {
                ValueKind::Ident => {
                    let val = factor.val.iter().collect::<String>();
                    if let Some(val) = self.name_map.get(&val) {
                        self.v = *val;
                    } else {
                        panic!("Variable not found");
                    }
                }
                _ => {
                    let val = factor.val.iter().collect::<String>();
                    let intval = val.parse::<i64>().expect("Invalid integer");
                    self.v = self.int32_ty.const_int(intval as u64, true).into();
                }
            },
            Ast::WithDecl(node) => {
                let read_ftype = self.int32_ty.fn_type(&[self.ptr_ty.into()], false);
                let read_fn =
                    self.module
                        .add_function("calc_read", read_ftype, Some(Linkage::External));

                for var in &node.vars {
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
                        .insert(var, call.try_as_basic_value().left().unwrap());
                }
                if let Some(mut expr) = node.expr_index {
                    expr.accept(exprs, self)?;
                }
            }
            Ast::Index(_) => {}
        }
        Ok(())
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn AstTrait<'a>) -> Result<()> {
        let mut tmp = ast.take();
        let res = self.inner_visit(exprs, &mut tmp);
        ast.replace(tmp);
        res
    }
}

#[derive(Debug, Default)]
pub struct DeclCheck<'a> {
    pub scope: HashSet<&'a [char]>,
    pub has_error: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorType {
    Twice,
    Not,
}

impl<'a> DeclCheck<'a> {
    pub fn new() -> DeclCheck<'a> {
        DeclCheck {
            scope: HashSet::new(),
            has_error: false,
        }
    }

    pub fn error(&mut self, err: ErrorType, s: &[char]) {
        let tmp = if err == ErrorType::Twice {
            "twice"
        } else {
            "not"
        };
        println!("Variable {:?} is {} declared", s, tmp);
        self.has_error = true;
    }
}

impl<'a> AstVisitorTrait<'a> for DeclCheck<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, node: &mut Ast<'a>) -> Result<()> {
        match node {
            Ast::BinaryOp(node) => {
                // TODO: check for existence of ExprIndex in exprs
                for scan in [node.lhs_expr, node.rhs_expr].iter_mut() {
                    if let Some(mut expr) = scan {
                        expr.accept(exprs, self)?;
                    } else {
                        self.has_error = true;
                    }
                }
            }
            Ast::Factor(node) => {
                if node.kind == ValueKind::Ident && !self.scope.contains(node.val) {
                    self.error(ErrorType::Not, node.val);
                }
            }
            Ast::WithDecl(node) => {
                for &i in &node.vars {
                    if self.scope.contains(i) {
                        self.error(ErrorType::Twice, i);
                        bail!("Variable declared twice");
                    }
                    self.scope.insert(i);
                }

                if let Some(mut expr) = node.expr_index {
                    expr.accept(exprs, self)?;
                } else {
                    self.has_error = true;
                }
            }
            Ast::Index(_) => {}
        }
        Ok(())
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn AstTrait<'a>) -> Result<()> {
        let mut tmp = ast.take();
        let res = self.inner_visit(exprs, &mut tmp);
        ast.replace(tmp);
        res
    }
}
