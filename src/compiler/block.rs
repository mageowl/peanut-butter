use std::{collections::HashMap, mem};

use super::{
    Compiler, Constant, Variable,
    comp_type::{CompType, MaybeUnknownType, Parameter},
    expression::ExpressionContext,
    writer::{OpPop, OpProperty},
};
use crate::{
    arch::uarch,
    error::{Error, Result},
    parser::statement::{Block, ConstantDefinition, FunctionDefinition, Statement},
    span::Chunk,
    vm::bytecode::OpCode,
};

pub struct BlockContext<'a> {
    pub return_type: Option<MaybeUnknownType<'a>>, // TODO: convert to reference
    pub expected_tail_type: MaybeUnknownType<'a>,
    pub const_scope: bool,
    pub extra_local_vars: uarch,
}

impl<'a> Compiler<'a> {
    fn assert_name_available(&self, name: Chunk<&'a str>, check_variables: bool) -> Result<()> {
        if check_variables && self.variables.contains_key(name.data) {
            return Err(Error::new(
                format!("The identifier '{}' is already in use.", name.data),
                name.span,
            ));
        }
        if self.constants.contains_key(name.data) {
            return Err(Error::new(
                format!("The identifier '{}' is already in use.", name.data),
                name.span,
            ));
        }
        Ok(())
    }

    pub(super) fn compile_const_def(
        &mut self,
        def: ConstantDefinition<'a>,
    ) -> Result<Constant<'a>> {
        let val_type = if let Some(ty_expr) = def.val_type {
            let ty_span = ty_expr.span;
            let (type_id, _) =
                self.execute_const_expr(ty_expr, MaybeUnknownType::Known(CompType::Type))?;
            let ty = self
                .get_type(type_id)
                .map_err(|()| Error::new("Invalid type.", ty_span))?;
            Some(ty)
        } else {
            None
        };

        let (value, val_type) = self.execute_const_expr(
            def.value,
            val_type.map_or(MaybeUnknownType::Infer, |t| {
                MaybeUnknownType::Known(t.clone())
            }),
        )?;

        if matches!(val_type, CompType::Type) {
            match self.get_type(value.clone()) {
                Ok(CompType::Struct(ptr)) => {
                    let _ = ptr.0.name.set(def.name.data);
                }
                Ok(CompType::Enum(ptr)) => {
                    let _ = ptr.0.name.set(def.name.data);
                }
                _ => (),
            }
        }

        let offset = self.constant_data.len() as uarch;
        self.constant_data.extend(value);
        Ok(Constant { val_type, offset })
    }

    pub(super) fn compile_block(
        &mut self,
        block: Chunk<Block<'a>>,
        ctx: BlockContext<'a>,
    ) -> Result<CompType<'a>> {
        let mut local_var_size: uarch = ctx.extra_local_vars;
        let mut local_vars = Vec::new();
        let mut local_consts = Vec::new();

        for statement in block.data.body {
            match statement.data {
                Statement::DefVariable {
                    name,
                    val_type,
                    value,
                } => {
                    self.assert_name_available(name, false)?;
                    let initialized = value.is_some();
                    let val_type = if let Some(ty_expr) = val_type {
                        let expr_span = ty_expr.span;
                        let (type_id, _) = self
                            .execute_const_expr(ty_expr, MaybeUnknownType::Known(CompType::Type))?;
                        let val_type = self
                            .get_type(type_id)
                            .map_err(|()| Error::new("Invalid type", expr_span))?
                            .clone();
                        if let Some(value) = value {
                            self.compile_expr(
                                value,
                                ExpressionContext {
                                    return_type: ctx.return_type.clone(),
                                    expected_type: MaybeUnknownType::Known(val_type.clone()),
                                    const_scope: ctx.const_scope,
                                },
                            )?;
                        }
                        val_type
                    } else if let Some(value) = value {
                        self.compile_expr(
                            value,
                            ExpressionContext {
                                return_type: ctx.return_type.clone(),
                                expected_type: MaybeUnknownType::Infer,
                                const_scope: ctx.const_scope,
                            },
                        )?
                    } else {
                        return Err(Error::new(
                            "Uninitialized variables must have a type.",
                            name.span,
                        ));
                    };
                    let val_size = val_type.size();

                    self.variables.insert(
                        name.data,
                        Variable {
                            val_type,
                            offset: self.variable_size,
                            initialized,
                        },
                    );
                    self.variable_size += val_size;
                    local_var_size += val_size;
                    local_vars.push(name.data);
                }
                Statement::DefConst(def) => {
                    self.assert_name_available(def.name, true)?;
                    local_consts.push(def.name.data);
                    let name = def.name.data;
                    let constant = self.compile_const_def(def)?;
                    self.constants.insert(name, constant);
                }
                Statement::DefFunction(def) => {
                    self.assert_name_available(def.name, true)?;
                    local_consts.push(def.name.data);
                    let name = def.name.data;
                    let constant = self.compile_fn_def(def)?;
                    self.constants.insert(name, constant);
                }

                Statement::Expr(expr) => {
                    let ty = self.compile_expr(
                        Chunk::new(statement.span, expr),
                        ExpressionContext {
                            return_type: ctx.return_type.clone(),
                            expected_type: MaybeUnknownType::Infer,
                            const_scope: ctx.const_scope,
                        },
                    )?;
                    self.instructions.push(OpPop { size: ty.size() });
                }
                _ => todo!(),
            }
        }

        let tail_ty = if let Some(tail) = block.data.tail {
            self.compile_expr(
                tail.unbox(),
                ExpressionContext {
                    return_type: ctx.return_type.clone(),
                    expected_type: ctx.expected_tail_type,
                    const_scope: ctx.const_scope,
                },
            )?
        } else {
            CompType::Tuple(vec![])
        };

        // Remove the local variables, but keep the tail value.
        self.instructions.push(OpProperty {
            struct_size: local_var_size + tail_ty.size(),
            property_offset: local_var_size,
            property_size: tail_ty.size(),
        });

        for name in local_vars {
            self.variables.remove(name);
        }
        for name in local_consts {
            self.constants.remove(name);
        }

        Ok(tail_ty)
    }
}
