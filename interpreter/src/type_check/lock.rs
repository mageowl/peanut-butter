use std::rc::Rc;

use super::{function::Function, Body, UnlockedScope};
use crate::parser::{expression::Expression, program::Program, statement::Statement};
use pbscript_lib::{error::Warn, module_tree::Scope, value::Value};

impl UnlockedScope {
    #[allow(private_bounds)]
    pub fn lock(mut self, tree: &mut Program) -> Vec<Warn> {
        let rc = Rc::new(Scope {
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

fn lock_body(parent: Rc<Scope>, tree: &mut impl Body, warnings: &mut Vec<Warn>) {
    for item in tree.body() {
        lock_statement(parent.clone(), &mut item.data, warnings);
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
            ..
        } => {
            let mut body_expr = body.take().expect("already locked");

            lock_expression(parent.clone(), &mut body_expr.data, warnings);
            let fn_ref = Value::Function(Rc::new(Function {
                body: body_expr,
                parameters: parameters.iter().map(|p| p.name.data.clone()).collect(),
                parent: parent.clone(),
                name: name.data.clone(),
            }));
            parent.variables[&name.data].value.replace(Some(fn_ref));
        }
        Statement::Expression(expr) => lock_expression(parent, expr, warnings),
    }
}

fn lock_expression(parent: Rc<Scope>, expression: &mut Expression, warnings: &mut Vec<Warn>) {
    match expression {
        Expression::Table(map) => {
            for (_, expr) in map {
                lock_expression(parent.clone(), &mut expr.data, warnings);
            }
        }
        Expression::Lambda { body, .. } => lock_expression(parent, &mut body.data, warnings),
        Expression::Reference(value) => lock_expression(parent, &mut value.data, warnings),

        Expression::Access(expr, _) | Expression::Index(expr, _) => {
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

        Expression::Block {
            scope,
            unlocked_scope,
            ..
        } => {
            let mut unlocked_scope = unlocked_scope
                .take()
                .expect("Tried to lock uninitialized scope");
            warnings.append(&mut unlocked_scope.warnings);
            *scope = Some(Rc::new(Scope {
                variables: unlocked_scope.variables,
                parent: None,
            }));
        }
        Expression::If { blocks, else_block } => {
            for (cond, block) in blocks {
                lock_expression(parent.clone(), &mut cond.data, warnings);
                lock_body(parent.clone(), &mut block.data, warnings);
            }
            if let Some(block) = else_block {
                lock_body(parent, &mut block.data, warnings);
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
