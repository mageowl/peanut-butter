use std::collections::HashMap;

use super::comp_type::{CompType, MaybeUnknownType};
use crate::{
    error::{Error, Result},
    parser::{
        expression::Expression,
        statement::{Block, ConstantDefinition, Statement},
    },
    span::Chunk,
};

struct Scope<'a, 'b> {
    variable_lookup: HashMap<&'a str, usize>,
    variables: Vec<Variable<'a>>,
    constants: HashMap<&'a str, Constant<'a>>,
    parent: Option<&'b Scope<'a, 'b>>,
}

struct Variable<'a> {
    name: &'a str,
    val_type: MaybeUnknownType<'a>,
    initialized: bool, // TODO: branching
}

struct Constant<'a> {
    val_type: MaybeUnknownType<'a>,
    offset: usize,
}

impl<'a> Scope<'a, '_> {
    pub fn visit_block(
        block: Chunk<&Block<'a>>,
        parent: Option<&Self>,
    ) -> Result<MaybeUnknownType<'a>> {
        let mut scope: Scope<'a, '_> = Scope {
            variables: Vec::new(),
            variable_lookup: HashMap::new(),
            constants: HashMap::new(),
            parent,
        };

        for statement in &block.data.body {
            match &statement.data {
                Statement::DefVariable {
                    name,
                    val_type: type_annotation,
                    value,
                } => {
                    let val_type = if let Some(type_annotation) = type_annotation {
                        let ty = scope.visit_expression(type_annotation.as_ref())?;
                        if !ty.matches_known(&CompType::Type) {
                            return Err(Error::new(
                                format!("Expected a type, but instead got {}.", ty.name()),
                                type_annotation.span,
                            ));
                        }
                        todo!("get type via VM")
                    } else if let Some(value) = value {
                        scope.visit_expression(value.as_ref())?
                    } else {
                        MaybeUnknownType::Infer
                    };

                    scope
                        .variable_lookup
                        .insert(name.data, scope.variables.len());
                    scope.variables.push(Variable {
                        name: name.data,
                        val_type,
                        initialized: value.is_some(),
                    });
                }
                Statement::DefConst(ConstantDefinition {
                    name,
                    val_type,
                    value,
                }) => {
                    let mut expr_type = scope.visit_expression(value.as_ref())?;
                    if let Some(ty) = val_type {
                        todo!("get type via VM.");
                        // if expr_type.matches(ty) {
                        //     expr_type.extend(ty);
                        // } else {
                        //     return Err(Error::new(format!(
                        //         "Expected {}, but instead got {}.",
                        //         expr_type.name(),
                        //         ty.name()
                        //     )));
                        // }
                    }

                    todo!("calculate value somehow");
                }
                _ => todo!(),
            }
        }

        if let Some(tail) = &block.data.tail {
            scope.visit_expression(tail.as_reborrow())
        } else {
            Ok(MaybeUnknownType::Known(CompType::Tuple(vec![])))
        }
    }

    pub fn visit_expression(
        &mut self,
        expr: Chunk<&Expression<'a>>,
    ) -> Result<MaybeUnknownType<'a>> {
        match expr.data {}
    }
}
