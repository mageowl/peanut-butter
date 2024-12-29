use std::{cell::RefCell, rc::Rc};

use super::{function::Function, Body, UnlockedScope};
use crate::parser::{block::Block, expression::Expression, program::Program, statement::Statement};
use hashbrown::HashMap;
use pbscript_lib::{
    error::Warn,
    module_tree::{Scope, Variable},
    types::Type,
    value::Value,
};

impl UnlockedScope {
    #[allow(private_bounds)]
    pub fn lock(mut self, tree: &mut Program) -> Vec<Warn> {
        let rc = Rc::new(Scope {
            imported_constants: self.imported_constants,
            variables: self.variables,
            parent: None,
        });

        for item in tree.body() {
            lock_statement(rc.clone(), &mut item.data, &mut self.warnings);
        }

        tree.scope = Some(rc);
        self.warnings
    }
}

fn lock_block(parent: Rc<Scope>, block: &mut Block, warnings: &mut Vec<Warn>) {
    let mut unlocked_scope = block
        .unlocked_scope
        .take()
        .expect("Tried to lock uninitialized scope");
    warnings.append(&mut unlocked_scope.warnings);
    block.scope = Some(Rc::new(Scope {
        imported_constants: unlocked_scope.imported_constants,
        variables: unlocked_scope.variables,
        parent: Some(parent),
    }));

    for item in &mut block.body {
        #[allow(clippy::unwrap_used)]
        lock_statement(block.scope.clone().unwrap(), &mut item.data, warnings);
    }
    if let Some(tail) = block.tail.as_mut() {
        #[allow(clippy::unwrap_used)]
        lock_expression(block.scope.clone().unwrap(), tail, warnings);
    }
}

fn lock_statement(parent: Rc<Scope>, statement: &mut Statement, warnings: &mut Vec<Warn>) {
    match statement {
        Statement::DefType { .. } => (),
        Statement::DefVariable { value, .. } => {
            if let Some(value) = value {
                lock_expression(parent, &mut value.data, warnings);
            }
        }
        Statement::DefFunction {
            body,
            name,
            parameters,
            return_type,
            ..
        } => {
            let mut body_expr = body.take().expect("already locked");
            let call_scope = Rc::new(Scope {
                imported_constants: HashMap::new(),
                variables: HashMap::from_iter(parameters.iter().map(|p| p.name.data.clone()).map(
                    |p| {
                        (
                            p,
                            Variable {
                                value_type: Type::Unit,
                                value: Rc::new(RefCell::new(Value::Unit)),
                                initialized: true,
                                mutable: false,
                            },
                        )
                    },
                )),
                parent: Some(parent.clone()),
            });

            lock_expression(call_scope.clone(), &mut body_expr.data, warnings);
            let args_signature = parameters
                .iter()
                .map(|p| format!("{name}: {ty:?}", name = p.name.data, ty = p.type_hint.data))
                .collect::<Vec<_>>()
                .join(", ");
            let fn_ref = Value::Function(Rc::new(Function {
                body: body_expr.span.with(body_expr.data),
                parameters: parameters.iter().map(|p| p.name.data.clone()).collect(),
                call_scope,
                signature: if let Some(return_ty) = return_type {
                    format!(
                        "fn {name}({args}) -> {return_ty:?}",
                        name = name,
                        args = args_signature,
                        return_ty = return_ty.data
                    )
                } else {
                    format!("fn {name}({args})", name = name, args = args_signature)
                },
            }));
            parent.variables[&name.data].value.replace(fn_ref);
        }
        Statement::Expression(expr) => lock_expression(parent, expr, warnings),
        Statement::Assign { value, target, .. } => {
            lock_expression(parent.clone(), value, warnings);
            lock_expression(parent, target, warnings);
        }
    }
}

fn lock_expression(parent: Rc<Scope>, expression: &mut Expression, warnings: &mut Vec<Warn>) {
    match expression {
        Expression::Table(map) => {
            for (_, expr) in map {
                lock_expression(parent.clone(), &mut expr.data, warnings);
            }
        }
        Expression::Lambda {
            body,
            parameters,
            value,
            return_type,
            ..
        } => {
            let mut body = body.take().expect("already locked");
            let call_scope = Rc::new(Scope {
                imported_constants: HashMap::new(),
                variables: HashMap::from_iter(parameters.iter().map(|p| p.name.data.clone()).map(
                    |p| {
                        (
                            p,
                            Variable {
                                value_type: Type::Unit,
                                value: Rc::new(RefCell::new(Value::Unit)),
                                initialized: true,
                                mutable: false,
                            },
                        )
                    },
                )),
                parent: Some(parent.clone()),
            });

            lock_expression(call_scope.clone(), &mut body.data, warnings);
            let args_signature = parameters
                .iter()
                .map(|p| format!("{name}: {ty:?}", name = p.name.data, ty = p.type_hint.data))
                .collect::<Vec<_>>()
                .join(", ");
            let fn_ref = Value::Function(Rc::new(Function {
                body: body.span.with(*body.data),
                parameters: parameters.iter().map(|p| p.name.data.clone()).collect(),
                call_scope,
                signature: if let Some(return_ty) = return_type.as_ref() {
                    format!(
                        "fn({args}) -> {return_ty:?}",
                        args = args_signature,
                        return_ty = return_ty.data
                    )
                } else {
                    format!("fn({args})", args = args_signature)
                },
            }));
            let _ = value.insert(fn_ref);
        }
        Expression::Reference(value) => lock_expression(parent, &mut value.data, warnings),

        Expression::Access(expr, _) => {
            lock_expression(parent, &mut expr.data, warnings);
        }
        Expression::DynAccess(target, prop) => {
            lock_expression(parent.clone(), &mut target.data, warnings);
            lock_expression(parent.clone(), &mut prop.data, warnings);
        }
        Expression::Call { value, args } => {
            for arg in args {
                lock_expression(parent.clone(), &mut arg.data, warnings);
            }
            lock_expression(parent, &mut value.data, warnings);
        }

        Expression::Block(block) => lock_block(parent, block, warnings),
        Expression::If { blocks, else_block } => {
            for (cond, block) in blocks {
                lock_expression(parent.clone(), &mut cond.data, warnings);
                lock_block(parent.clone(), block, warnings);
            }
            if let Some(block) = else_block {
                lock_block(parent, block, warnings);
            }
        }

        Expression::BinaryOp { a, b, .. } => {
            lock_expression(parent.clone(), &mut a.data, warnings);
            lock_expression(parent, &mut b.data, warnings);
        }
        Expression::UnaryOp { a, .. } => {
            lock_expression(parent, &mut a.data, warnings);
        }

        _ => (),
    }
}
