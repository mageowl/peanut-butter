use std::collections::HashMap;

use comp_type::CompType;

use crate::{uarch, vm::VirtualMachine};

pub mod comp_type;
//pub mod type_checker;
pub mod expression;

pub struct Compiler<'a, const CONST_STACK_SIZE: usize> {
    variables: HashMap<&'a str, Variable<'a>>,
    variable_size: uarch,

    constants: HashMap<&'a str, Constant<'a>>,
    literals: HashMap<Literal<'a>, uarch>,
    constant_size: uarch,
    constant_data: Vec<u8>,

    instructions: Vec<u8>,
}

struct Variable<'a> {
    val_type: CompType<'a>,
    offset: uarch,
    initialized: bool,
}

struct Constant<'a> {
    val_type: CompType<'a>,
    offset: uarch,
}

enum Literal<'a> {
    String(&'a str),
    Boolean(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl<'a, const CONST_STACK_SIZE: usize> Compiler<'a, CONST_STACK_SIZE> {
    pub fn new() -> Self {
        assert!(CONST_STACK_SIZE < uarch::MAX as usize);

        Self {
            variables: HashMap::new(),
            variable_size: 0,
            constants: HashMap::new(),
            literals: HashMap::new(),
            constant_size: 0,
            constant_data: Vec::new(),
            instructions: Vec::new(),
        }
    }
}
