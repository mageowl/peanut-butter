use pbscript_lib::{instruction::Reporter, span::Chunk, types::Type};

use crate::parser::expression::Expression;

use super::Scope;

pub fn compile_expression(expression: Chunk<Expression>, scope: &mut Scope) -> (Type, Reporter) {
    todo!()
}
