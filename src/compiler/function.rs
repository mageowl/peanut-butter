use std::{collections::HashMap, mem};

use super::{
    Compiler, Constant, Variable,
    block::BlockContext,
    comp_type::{CompType, MaybeUnknownType, Parameter},
    writer::OpReturn,
};
use crate::{
    arch::uarch,
    error::{Error, Result},
    parser::statement::FunctionDefinition,
};

impl<'a> Compiler<'a> {
    pub(super) fn compile_fn_def(&mut self, def: FunctionDefinition<'a>) -> Result<Constant<'a>> {
        let old_instructions = mem::replace(&mut self.instructions.data, Vec::new());
        let old_variables = mem::replace(&mut self.variables, HashMap::new());
        let old_variable_size = mem::replace(&mut self.variable_size, 0);

        let parameters: Vec<_> = def
            .parameters
            .into_iter()
            .map(|p| {
                if self.variables.contains_key(p.data.name.data) {
                    return Err(Error::new(
                        format!("Parameter '{}' declared more than once.", p.data.name.data),
                        p.data.name.span,
                    ));
                }

                let (type_id, _) =
                    self.execute_const_expr(p.data.ty, MaybeUnknownType::Known(CompType::Type))?;
                let ty = self
                    .get_type(type_id)
                    .map_err(|()| Error::new("Invalid type.", p.span))?
                    .clone();

                self.variables.insert(
                    p.data.name.data,
                    Variable {
                        val_type: ty.clone(),
                        offset: self.variable_size,
                        initialized: true,
                    },
                );
                println!("added variable");
                self.variable_size += ty.size();

                Ok(Parameter {
                    name: p.data.name.data,
                    ty,
                })
            })
            .collect::<Result<_>>()?;
        let parameter_size = self.variable_size;

        let return_type = Box::new(if let Some(ty) = def.return_type {
            let ty_span = ty.span;
            let (type_id, _) =
                self.execute_const_expr(ty, MaybeUnknownType::Known(CompType::Type))?;
            self.get_type(type_id)
                .map_err(|()| Error::new("Invalid type.", ty_span))?
                .clone()
        } else {
            CompType::Tuple(vec![])
        });

        self.compile_block(
            def.body,
            BlockContext {
                return_type: Some(MaybeUnknownType::Known(*return_type.clone())),
                expected_tail_type: MaybeUnknownType::Known(*return_type.clone()),
                const_scope: false,
                extra_local_vars: parameter_size,
            },
        )?;
        self.instructions.push(OpReturn);

        let offset = self.constant_data.len() as uarch;
        self.constant_data
            .extend((self.function_data.len() as uarch).to_le_bytes());

        self.function_data.append(&mut self.instructions.data);

        self.instructions.data = old_instructions;
        self.variables = old_variables;
        self.variable_size = old_variable_size;
        Ok(Constant {
            val_type: CompType::Function {
                parameters,
                return_type,
            },
            offset,
        })
    }
}
