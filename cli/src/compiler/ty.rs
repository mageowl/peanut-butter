use pbscript_lib::{error::Result, span::Chunk, types::Type};

use crate::parser::type_name::TypeName;

use super::Scope;

pub fn compile_ty(type_name: Chunk<TypeName>, scope: &mut Scope) -> Result<Type> {
    todo!()
}
