use crate::{
    arch::{iarch, uarch},
    compiler::{
        Compiler,
        comp_type::CompType,
        writer::{OpConstant, OpProperty, OpStackCopy},
    },
    error::{Error, Result},
    parser::expression::Expression,
    span::Chunk,
};

use super::ExpressionContext;

enum Source {
    Const,
    LocalVar,
    Stack(uarch),
}

struct PropertyData<'a> {
    source: Source,
    offset: uarch,
    val_type: CompType<'a>,
}

impl<'a> PropertyData<'a> {
    fn add_field(&mut self, name: Chunk<&str>) -> Result<()> {
        let target_struct = match &self.val_type {
            CompType::Struct(ptr) => ptr,
            _ => {
                return Err(Error::new(
                    "Non-struct types cannot have properties.",
                    name.span,
                ));
            }
        };
        let field = match target_struct.0.fields.get(name.data) {
            Some(f) => f,
            None => {
                return Err(Error::new(
                    format!(
                        "No field '{}' exists on {}",
                        name.data,
                        target_struct.0.name()
                    ),
                    name.span,
                ));
            }
        };
        self.offset += field.offset;
        self.val_type = field.field_type.clone(); // TODO: no clone maybe?
        Ok(())
    }
}

impl<'a> Compiler<'a> {
    fn get_property_data(
        &mut self,
        target: Chunk<Expression<'a>>,
        ctx: ExpressionContext<'a>,
    ) -> Result<PropertyData<'a>> {
        match target {
            Chunk {
                data: Expression::Ident(name),
                span,
            } => {
                if let Some(var) = self.variables.get(name) {
                    if !var.initialized {
                        return Err(Error::new(format!("'{name}' is not initialized."), span));
                    }
                    Ok(PropertyData {
                        source: Source::LocalVar,
                        offset: var.offset,
                        val_type: var.val_type.clone(),
                    })
                } else if let Some(constant) = self.constants.get(name) {
                    Ok(PropertyData {
                        source: Source::Const,
                        offset: constant.offset,
                        val_type: constant.val_type.clone(),
                    })
                } else {
                    Err(Error::new(format!("Unknown variable '{name}'."), span))
                }
            }
            Chunk {
                data: Expression::Property(target, name),
                span: _,
            } => {
                let mut prop_data = self.get_property_data(target.unbox(), ctx)?;
                prop_data.add_field(name)?;
                Ok(prop_data)
            }
            expr => {
                let val_type = self.compile_expr(expr, ctx)?;
                Ok(PropertyData {
                    source: Source::Stack(val_type.size()),
                    offset: 0,
                    val_type: val_type.clone(),
                })
            }
        }
    }

    pub fn compile_property(
        &mut self,
        target: Chunk<Expression<'a>>,
        name: Chunk<&'a str>,
        ctx: ExpressionContext<'a>,
    ) -> Result<CompType<'a>> {
        let mut prop_data = self.get_property_data(target, ctx)?;
        prop_data.add_field(name)?;

        match prop_data.source {
            Source::Const => {
                self.instructions.push(OpConstant {
                    offset: prop_data.offset,
                    size: prop_data.val_type.size(),
                });
            }
            Source::LocalVar => {
                self.instructions.push(OpStackCopy {
                    offset: prop_data.offset as iarch,
                    size: prop_data.val_type.size(),
                });
            }
            Source::Stack(struct_size) => {
                self.instructions.push(OpProperty {
                    struct_size,
                    property_offset: prop_data.offset,
                    property_size: prop_data.val_type.size(),
                });
            }
        }

        Ok(prop_data.val_type)
    }
}
