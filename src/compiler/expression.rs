use std::{cell::OnceCell, collections::HashMap, iter, mem, sync::Arc};

use super::{
    Compiler, Constant, Literal,
    block::BlockContext,
    comp_type::{
        CompType, EnumPointer, EnumType, Field, MaybeUnknownType, NumberType, StructPointer,
        StructType, Variant,
    },
    min_bytes,
    writer::{
        OpArithmatic, OpComparison, OpEqual, OpJumpIfZero, OpNegate, OpPushZeroed, OpStackWrite,
    },
};
use crate::{
    arch::{iarch, uarch},
    compiler::writer::{OpConstant, OpStackCopy},
    error::{Error, Result},
    parser::{
        expression::{BooleanOperation, ComparisonOperation, Expression, Operation},
        statement::{ConstantDefinition, FunctionDefinition},
    },
    span::Chunk,
    vm::bytecode::OpCode,
};

pub mod property;

#[derive(Clone)]
pub struct ExpressionContext<'a> {
    pub return_type: Option<MaybeUnknownType<'a>>,
    pub expected_type: MaybeUnknownType<'a>,
    pub const_scope: bool,
}

impl<'a> Compiler<'a> {
    fn compile_literal(&mut self, literal: Literal, lit_type: &CompType) {
        let offset = match self.literals.get(&literal) {
            Some(offset) => *offset,
            None => {
                let offset = self.constant_data.len() as uarch;
                literal.append_bytes_to(&mut self.constant_data);
                offset
            }
        };
        self.instructions.push(OpConstant {
            offset,
            size: lit_type.size(),
        });
    }

    fn create_assocs(
        &mut self,
        assoc_consts: Vec<Chunk<ConstantDefinition<'a>>>,
        assoc_fns: Vec<Chunk<FunctionDefinition<'a>>>,
    ) -> Result<HashMap<&'a str, Constant<'a>>> {
        let mut names: Vec<&str> = Vec::new();
        let mut constants: HashMap<&str, Constant<'a>> = assoc_consts
            .into_iter()
            .map(|def| {
                if names.contains(&def.data.name.data) {
                    return Err(Error::new(
                        format!("Constant '{}' declared more than once.", def.data.name.data),
                        def.data.name.span,
                    ));
                } else {
                    names.push(def.data.name.data);
                }

                let name = def.data.name.data;
                let constant = self.compile_const_def(def.data)?;

                Ok((name, constant))
            })
            .collect::<Result<_>>()?;

        for def in assoc_fns {
            if names.contains(&def.data.name.data) {
                return Err(Error::new(
                    format!("Function '{}' declared more than once.", def.data.name.data),
                    def.data.name.span,
                ));
            } else {
                names.push(def.data.name.data);
            }

            let name = def.data.name.data;
            let constant = self.compile_fn_def(def.data)?;

            constants.insert(name, constant);
        }

        Ok(constants)
    }

    pub(super) fn compile_expr(
        &mut self,
        expr: Chunk<Expression<'a>>,
        ctx: ExpressionContext<'a>,
    ) -> Result<CompType<'a>> {
        let ty = match expr.data {
            Expression::Null => panic!("null needs to get removed"),
            Expression::String(str) => {
                let ty = CompType::StringSlice;
                self.compile_literal(Literal::String(str), &ty);
                ty
            }
            Expression::Int {
                slice,
                negative: _,
                type_spec,
            } => {
                let num_type = if let Some(type_spec) = type_spec {
                    type_spec.data
                } else if let MaybeUnknownType::Known(CompType::Number(n)) = ctx.expected_type {
                    n
                } else {
                    return Err(Error::new(
                        "Failed to infer this number's concrete type. Try adding a type specifier",
                        expr.span,
                    ));
                };
                let literal = match num_type {
                    NumberType::U8 => Literal::U8(slice.parse::<u8>().map_err(|e| {
                        Error::new(format!("Failed to parse an integer: {e}"), expr.span)
                    })?),
                    NumberType::U16 => Literal::U16(slice.parse::<u16>().map_err(|e| {
                        Error::new(format!("Failed to parse an integer: {e}"), expr.span)
                    })?),
                    NumberType::U32 => Literal::U32(slice.parse::<u32>().map_err(|e| {
                        Error::new(format!("Failed to parse an integer: {e}"), expr.span)
                    })?),
                    NumberType::U64 => Literal::U64(slice.parse::<u64>().map_err(|e| {
                        Error::new(format!("Failed to parse an integer: {e}"), expr.span)
                    })?),
                    // SAFETY: this is okay because all possible values are valid integers.
                    NumberType::I8 => Literal::U8(unsafe {
                        mem::transmute(slice.parse::<i8>().map_err(|e| {
                            Error::new(format!("Failed to parse an integer: {e}"), expr.span)
                        })?)
                    }),
                    // SAFETY: this is okay because all possible values are valid integers.
                    NumberType::I16 => Literal::U16(unsafe {
                        mem::transmute(slice.parse::<i16>().map_err(|e| {
                            Error::new(format!("Failed to parse an integer: {e}"), expr.span)
                        })?)
                    }),
                    // SAFETY: this is okay because all possible values are valid integers.
                    NumberType::I32 => Literal::U32(unsafe {
                        mem::transmute(slice.parse::<i32>().map_err(|e| {
                            Error::new(format!("Failed to parse an integer: {e}"), expr.span)
                        })?)
                    }),
                    // SAFETY: this is okay because all possible values are valid integers.
                    NumberType::I64 => Literal::U64(unsafe {
                        mem::transmute(slice.parse::<i64>().map_err(|e| {
                            Error::new(format!("Failed to parse an integer: {e}"), expr.span)
                        })?)
                    }),

                    _ => {
                        return Err(Error::new(
                            "Expected a float, but this looks like an integer. Consider adding a decimal.",
                            expr.span,
                        ));
                    }
                };

                let ty = CompType::Number(num_type);
                self.compile_literal(literal, &ty);
                ty
            }
            Expression::Float {
                slice,
                negative: _,
                type_spec,
            } => {
                let num_type: NumberType = if let Some(type_spec) = type_spec {
                    type_spec.data
                } else if let MaybeUnknownType::Known(CompType::Number(n)) = ctx.expected_type {
                    n
                } else {
                    return Err(Error::new(
                        "Failed to infer this number's concrete type. Try adding a type specifier",
                        expr.span,
                    ));
                };

                let literal = match num_type {
                    // SAFETY: this is okay because all possible values are valid integers.
                    NumberType::F32 => Literal::U32(unsafe {
                        mem::transmute(slice.parse::<f32>().map_err(|e| {
                            Error::new(format!("Failed to parse a float: {e}"), expr.span)
                        })?)
                    }),
                    NumberType::F64 => Literal::U64(unsafe {
                        mem::transmute(slice.parse::<f64>().map_err(|e| {
                            Error::new(format!("Failed to parse a float: {e}"), expr.span)
                        })?)
                    }),

                    _ => {
                        return Err(Error::new(
                            "Expected an integer, but this is a float.",
                            expr.span,
                        ));
                    }
                };

                let ty = CompType::Number(num_type);
                self.compile_literal(literal, &ty);
                ty
            }
            Expression::Boolean(b) => {
                let ty = CompType::Boolean;
                self.compile_literal(Literal::Boolean(b), &ty);
                ty
            }

            Expression::Ident(name) => {
                if let Some(var) = self.variables.get(name) {
                    if !var.initialized {
                        return Err(Error::new(
                            format!("'{name}' is not initialized."),
                            expr.span,
                        ));
                    }
                    self.instructions.push(OpStackCopy {
                        offset: var.offset.try_into().unwrap(),
                        size: var.val_type.size(),
                    });
                    var.val_type.clone()
                } else if let Some(constant) = self.constants.get(name) {
                    self.instructions.push(OpConstant {
                        offset: constant.offset,
                        size: constant.val_type.size(),
                    });
                    constant.val_type.clone()
                } else {
                    return Err(Error::new(format!("Unknown variable '{name}'."), expr.span));
                }
            }
            Expression::IdentSelf => todo!(),

            Expression::Property(target, name) => {
                self.compile_property(target.unbox(), name, ctx.clone())?
            }

            Expression::StructType {
                properties,
                methods,
                constants,
            } => {
                let mut field_names: Vec<&str> = Vec::new();
                let mut size: uarch = 0;
                let fields = properties
                    .into_iter()
                    .map(|(name, ty)| {
                        if field_names.contains(&name.data) {
                            return Err(Error::new(
                                format!("Field '{}' declared more than once.", name.data),
                                name.span,
                            ));
                        } else {
                            field_names.push(name.data);
                        }

                        let ty_span = ty.span;
                        let (ty_id, _) =
                            self.execute_const_expr(ty, MaybeUnknownType::Known(CompType::Type))?;
                        let field_type = self
                            .get_type(ty_id)
                            .map_err(|()| Error::new("Invalid type", ty_span))?
                            .clone();

                        let offset = size;
                        size += field_type.size();
                        Ok((name.data, Field { field_type, offset }))
                    })
                    .collect::<Result<_>>()?;

                let assoc_consts = self.create_assocs(constants, methods)?;

                let id = self.type_map.len() as uarch;
                self.type_map
                    .push(CompType::Struct(StructPointer(Arc::new(StructType {
                        name: OnceCell::new(),
                        fields,
                        assoc_consts,
                    }))));
                self.compile_literal(Literal::UArch(id), &CompType::Type);
                CompType::Type
            }
            Expression::EnumType {
                variants,
                methods,
                constants,
            } => {
                let mut variant_names: Vec<&str> = Vec::new();
                let variants: HashMap<&str, Variant<'a>> = variants
                    .into_iter()
                    .enumerate()
                    .map(|(tag, (name, ty))| {
                        if variant_names.contains(&name.data) {
                            return Err(Error::new(
                                format!("Field '{}' declared more than once.", name.data),
                                name.span,
                            ));
                        } else {
                            variant_names.push(name.data);
                        }

                        let inner_type = ty
                            .map(|ty| {
                                let ty_span = ty.span;
                                let (ty_id, _) = self.execute_const_expr(
                                    ty,
                                    MaybeUnknownType::Known(CompType::Type),
                                )?;
                                Ok(self
                                    .get_type(ty_id)
                                    .map_err(|()| Error::new("Invalid type", ty_span))?
                                    .clone())
                            })
                            .transpose()?;

                        Ok((
                            name.data,
                            Variant {
                                tag: tag as uarch,
                                inner_type,
                            },
                        ))
                    })
                    .collect::<Result<_>>()?;

                let assoc_consts = self.create_assocs(constants, methods)?;

                let id = self.type_map.len() as uarch;
                self.type_map
                    .push(CompType::Enum(EnumPointer(Arc::new(EnumType {
                        name: OnceCell::new(),
                        tag_bytes: min_bytes(variants.len() as uarch).ok_or_else(|| {
                            Error::new("This enum has too many variants.", expr.span)
                        })?,
                        variants,
                        assoc_consts,
                    }))));
                self.compile_literal(Literal::UArch(id), &CompType::Type);
                CompType::Type
            }

            Expression::StructInitializer {
                struct_type,
                properties,
            } => {
                let type_span = struct_type.span;
                let (type_id, _) = self.execute_const_expr(
                    struct_type.unbox(),
                    MaybeUnknownType::Known(CompType::Type),
                )?;
                let struct_type = match self
                    .get_type(type_id)
                    .map_err(|()| Error::new("Invalid type", type_span))?
                {
                    CompType::Struct(ptr) => ptr.clone(),
                    ty => {
                        return Err(Error::new(
                            format!("Expected a struct, but instead got {}", ty.name()),
                            type_span,
                        ));
                    }
                };

                if properties.len() < struct_type.0.fields.len() {
                    return Err(Error::new("Missing properties.", expr.span)); // TODO: which ones
                }

                self.instructions.push(OpPushZeroed {
                    size: struct_type.0.size(),
                });
                for (name, value) in properties {
                    let field = struct_type.0.fields.get(name.data).ok_or_else(|| {
                        Error::new(
                            format!(
                                "Field '{}' does not exist on {}.",
                                name.data,
                                struct_type.0.name()
                            ),
                            name.span,
                        )
                    })?;
                    self.compile_expr(
                        value,
                        ExpressionContext {
                            return_type: ctx.return_type.clone(),
                            expected_type: MaybeUnknownType::Known(field.field_type.clone()),
                            const_scope: ctx.const_scope,
                        },
                    )?;
                    self.instructions.push(OpStackWrite {
                        offset: field.offset as iarch
                            - (struct_type.0.size() as iarch)
                            - field.field_type.size() as iarch,
                        size: field.field_type.size(),
                    });
                }

                CompType::Struct(struct_type)
            }

            Expression::Block(block) => self.compile_block(
                Chunk::new(expr.span, block),
                BlockContext {
                    return_type: ctx.return_type.clone(),
                    expected_tail_type: ctx.expected_type.clone(),
                    const_scope: ctx.const_scope,
                },
            )?,

            Expression::BinaryOp { lhs, rhs, op } => match op.data {
                Operation::Concat => todo!(),
                Operation::Arithmatic(op) => {
                    let lhs_span = lhs.span;
                    let ty = self.compile_expr(
                        lhs.unbox(),
                        ExpressionContext {
                            return_type: ctx.return_type.clone(),
                            expected_type: MaybeUnknownType::Infer,
                            const_scope: ctx.const_scope,
                        },
                    )?;
                    match &ty {
                        CompType::Number(num_ty) => {
                            self.compile_expr(
                                rhs.unbox(),
                                ExpressionContext {
                                    return_type: ctx.return_type.clone(),
                                    expected_type: MaybeUnknownType::Known(CompType::Number(
                                        *num_ty,
                                    )),
                                    const_scope: ctx.const_scope,
                                },
                            )?;
                            self.instructions.push(OpArithmatic {
                                num_ty: *num_ty,
                                op,
                            });
                            CompType::Number(*num_ty)
                        }
                        CompType::Struct(ptr) => {
                            ptr.0.assoc_consts.get(op.impl_name());
                            todo!()
                        }
                        _ => {
                            return Err(Error::new(
                                format!("Cannot apply the addition operator {op} to {}", ty.name()),
                                lhs_span,
                            ));
                        }
                    }
                }
                Operation::Equals { inverted: invert } => {
                    let ty = self.compile_expr(
                        lhs.unbox(),
                        ExpressionContext {
                            return_type: ctx.return_type.clone(),
                            expected_type: MaybeUnknownType::Infer,
                            const_scope: ctx.const_scope,
                        },
                    )?;
                    let ty_size = ty.size();
                    self.compile_expr(
                        rhs.unbox(),
                        ExpressionContext {
                            return_type: ctx.return_type.clone(),
                            expected_type: MaybeUnknownType::Known(ty),
                            const_scope: ctx.const_scope,
                        },
                    )?;
                    self.instructions.push(OpEqual { ty_size, invert });
                    CompType::Boolean
                }
                Operation::Comparison(op) => {
                    let ty = self.compile_expr(
                        lhs.unbox(),
                        ExpressionContext {
                            return_type: ctx.return_type.clone(),
                            expected_type: MaybeUnknownType::Infer,
                            const_scope: ctx.const_scope,
                        },
                    )?;
                    match &ty {
                        CompType::Number(num_ty) => {
                            self.compile_expr(
                                rhs.unbox(),
                                ExpressionContext {
                                    return_type: ctx.return_type.clone(),
                                    expected_type: MaybeUnknownType::Known(CompType::Number(
                                        *num_ty,
                                    )),
                                    const_scope: ctx.const_scope,
                                },
                            )?;
                            self.instructions.push(OpComparison {
                                num_ty: *num_ty,
                                op,
                            });
                            if matches!(
                                op,
                                ComparisonOperation::LtEqual | ComparisonOperation::GtEqual
                            ) {
                                self.instructions.push(OpNegate);
                            }
                            CompType::Boolean
                        }
                        _ => todo!(),
                    }
                }
                Operation::Boolean(op) => {
                    let ctx = ExpressionContext {
                        return_type: ctx.return_type.clone(),
                        expected_type: MaybeUnknownType::Known(CompType::Boolean),
                        const_scope: ctx.const_scope,
                    };
                    self.compile_expr(lhs.unbox(), ctx.clone())?;
                    let slot = self.instructions.reserve::<OpJumpIfZero>();
                    self.compile_expr(rhs.unbox(), ctx)?;
                    self.instructions.fill_reserved(
                        OpJumpIfZero {
                            distance: (self.instructions.data.len() - slot.index) as uarch,
                            invert: op == BooleanOperation::Or,
                        },
                        slot,
                    );

                    CompType::Boolean
                }
            },
            Expression::Group(inner) => {
                // TODO: apply optimizations to groups as well (e.g. var.prop to one StackCopy)
                return self.compile_expr(Chunk::new(expr.span, *inner), ctx);
            }

            expr => {
                dbg!(expr);
                todo!();
            }
        };

        if !ctx.expected_type.matches_known(&ty) {
            Err(Error::new(
                format!(
                    "Expected {}, but got {}.",
                    ctx.expected_type.name(),
                    ty.name()
                ),
                expr.span,
            ))
        } else {
            Ok(ty)
        }
    }
}
