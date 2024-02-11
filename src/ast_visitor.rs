use std::collections::HashMap;

use enum_dispatch::enum_dispatch;
use inkwell::{
    builder::Builder,
    module::{Linkage, Module},
    values::{BasicValueEnum, IntMathValue},
    AddressSpace,
};

use crate::*;

#[derive(Debug)]
#[enum_dispatch]
pub enum ASTVisitor<'a> {
    DeclCheck(DeclCheck<'a>),
    ToIRVisitor(ToIRVisitor<'a>),
}

impl<'a> Default for ASTVisitor<'a> {
    fn default() -> Self {
        ASTVisitor::DeclCheck(DeclCheck::new())
    }
}

#[enum_dispatch(ASTVisitor)]
pub trait ASTVisitorTrait<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>);
    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn ASTTrait<'a>);
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
    _phantom: std::marker::PhantomData<&'ctx ()>,
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
            _phantom: std::marker::PhantomData,
        }
    }
    pub fn run(&mut self, exprs: &mut Vec<Expr<'ctx>>, tree: &mut AST<'ctx>) {
        let main_fn_type = self
            .int32_ty
            .fn_type(&[self.int32_ty.into(), self.ptr_ty.into()], false);
        let main_fn = self
            .module
            .add_function("main", main_fn_type, Some(Linkage::External));
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Assume tree has an accept method that takes a visitor
        tree.accept(exprs, self);

        let calc_write_fn_type = self.void_ty.fn_type(&[self.int32_ty.into()], false);
        let calc_write_fn =
            self.module
                .add_function("calc_write", calc_write_fn_type, Some(Linkage::External));
        self.builder
            .build_call(calc_write_fn, &[self.v.into()], "call_calc_write")
            .unwrap();

        self.builder.build_return(Some(&self.int32_zero)).unwrap();
    }
}

impl<'a> Default for ToIRVisitor<'a> {
    fn default() -> Self {
        todo!()
    }
}

impl<'a> ASTVisitorTrait<'a> for ToIRVisitor<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut AST<'a>) {
        let ast_owned = std::mem::take(ast);
        *ast = match ast_owned {
            AST::BinaryOp(node) => {
                if let Some(left_idx) = node.lhs_expr {
                    let left = exprs.get_mut(left_idx.0).unwrap();
                    let mut l = std::mem::take(left);
                    l.accept(exprs, self);
                    exprs[left_idx.0] = l;
                    let left = self.v;
                    if let Some(right_idx) = node.rhs_expr {
                        let right = exprs.get_mut(right_idx.0).unwrap();
                        let mut r = std::mem::take(right);
                        r.accept(exprs, self);
                        exprs[right_idx.0] = r;
                        let right = self.v;
                        self.v = match node.op {
                            Operator::Plus => self
                                .builder
                                .build_int_add(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "addtmp",
                                )
                                .unwrap()
                                .into(),
                            Operator::Minus => self
                                .builder
                                .build_int_sub(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "subtmp",
                                )
                                .unwrap()
                                .into(),
                            Operator::Mul => self
                                .builder
                                .build_int_mul(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "multmp",
                                )
                                .unwrap()
                                .into(),
                            Operator::Div => self
                                .builder
                                .build_int_signed_div(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "divtmp",
                                )
                                .unwrap()
                                .into(),
                        }
                    }
                }
                node.into()
            }
            AST::Factor(factor) => {
                match factor.kind {
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
                }
                factor.into()
            }
            AST::WithDecl(node) => {
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
                        &var,
                    );
                    global_str.set_initializer(&str_val);
                    let call = self
                        .builder
                        .build_call(
                            read_fn,
                            &[global_str.as_pointer_value().into()],
                            "read_call",
                        )
                        .unwrap();
                    self.name_map
                        .insert(var, call.try_as_basic_value().left().unwrap());
                }
                if let Some(expr) = node.expr_index {
                    let e = exprs.get_mut(expr.0).unwrap();
                    let mut e = std::mem::take(e);
                    e.accept(exprs, self);
                    exprs[expr.0] = e;
                }
                node.into()
            }
            AST::Index(index) => index.into(),
        }
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn ASTTrait<'a>) {
        let mut tmp = ast.take();
        self.inner_visit(exprs, &mut tmp);
        ast.replace(tmp);
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

    pub fn check(&self, _ast: AST<'a>) -> bool {
        self.has_error
    }
}

impl<'a> ASTVisitorTrait<'a> for DeclCheck<'a> {
    fn inner_visit(&mut self, exprs: &mut Vec<Expr<'a>>, node: &mut AST<'a>) {
        match node {
            AST::BinaryOp(node) => {
                // TODO: check for existence of ExprIndex in exprs
                if let Some(mut left) = node.lhs_expr {
                    left.accept(exprs, self);
                } else {
                    self.has_error = true;
                }
                if let Some(mut right) = node.rhs_expr {
                    right.accept(exprs, self);
                } else {
                    self.has_error = true;
                }
            }
            AST::Factor(node) => {
                if node.kind == ValueKind::Ident && !self.scope.contains(node.val) {
                    self.error(ErrorType::Not, node.val);
                }
            }
            AST::WithDecl(node) => {
                for &i in &node.vars {
                    if self.scope.contains(i) {
                        self.error(ErrorType::Twice, i);
                        return;
                    }
                    self.scope.insert(i);
                }

                if let Some(mut expr) = node.expr_index {
                    expr.accept(exprs, self);
                } else {
                    self.has_error = true;
                }
            }
            AST::Index(_) => {}
        }
    }

    fn visit(&mut self, exprs: &mut Vec<Expr<'a>>, ast: &mut dyn ASTTrait<'a>) {
        let mut tmp = ast.take();
        self.inner_visit(exprs, &mut tmp);
        ast.replace(tmp);
    }
}
