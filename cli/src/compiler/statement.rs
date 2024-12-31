use super::{expression::compile_expression, ty::compile_ty, Scope, Variable};
use crate::parser::{block::Block, expression::Expression, statement::Statement, Parameter};
use pbscript_lib::{
    error::{Error, Result},
    instruction::{Instruction, InstructionSet, Reporter},
    span::Chunk,
    types::Type,
};

pub fn compile_statement(statement: Chunk<Statement>, scope: &mut Scope) -> Result<()> {
    match statement.data {
        Statement::DefVariable {
            mutable,
            name,
            type_hint,
            value,
        } => {
            let idx = scope.variables.len();
            let (ty, rep, value_span) = match value {
                Some(v) => {
                    let span = v.span;
                    let (ty, rep) = compile_expression(v, scope);
                    (Some(ty), Some(rep), Some(span))
                }
                None => (None, None, None),
            };

            let type_hint = if let Some(hint) = type_hint {
                let hint_ty = compile_ty(hint, scope)?;
                if let (Some(ty), Some(value_span)) = (ty, value_span) {
                    if !hint_ty.matches(&ty) {
                        return Err(Error::new(
                            value_span,
                            "This expression does not match the type of the variable.",
                        ));
                    }
                }
                hint_ty
            } else if let Some(ty) = ty {
                ty
            } else {
                return Err(Error::new(
                    name.span,
                    "Variables need either a type or an initial value.",
                ));
            };

            scope.variables.insert(
                name.data,
                Variable {
                    ty: type_hint,
                    mutable,
                    initialized: rep.is_some(),
                    idx,
                },
            );
            scope.instructions.allocation += 1;

            if let Some(value) = rep {
                scope
                    .instructions
                    .push(Instruction::Set { up: 0, idx, value });
            }
            Ok(())
        }
        Statement::DefFunction {
            mutable,
            name,
            parameters,
            return_type,
            body,
        } => {
            let idx = scope.variables.len();
            let return_type =
                Box::new(return_type.map_or(Ok(Type::Unit), |rt| compile_ty(rt, scope))?);
            let body_span = body.span;
            let (instructions, expr_ty) = compile_fn(body, &parameters, scope)?;

            if !return_type.matches(&expr_ty) {
                return Err(Error::new(body_span, format!("This expression does not match the function signature. Expected a return type of {}.", return_type.simple_name())));
            }

            let ty = Type::Fn {
                return_type,
                parameters: parameters
                    .into_iter()
                    .map(|p| compile_ty(p.data.type_hint, scope))
                    .collect::<Result<_>>()?,
            };

            scope.variables.insert(
                name.data,
                Variable {
                    ty,
                    mutable,
                    initialized: true,
                    idx,
                },
            );
            scope.instructions.allocation += 1;
            scope.instructions.push(Instruction::Set {
                up: 0,
                idx,
                value: Reporter::Lambda(instructions),
            });

            Ok(())
        }
        Statement::DefType {
            name,
            generics,
            value,
        } => todo!(),
        Statement::Assign { target, value, op } => todo!(),
        Statement::Expression(expression) => todo!(),
    }
}

fn compile_fn(
    body: Chunk<Expression>,
    parameters: &Vec<Chunk<Parameter>>,
    parent: &mut Scope,
) -> Result<(InstructionSet, Type)> {
    todo!()
}
