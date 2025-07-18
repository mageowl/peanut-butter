use std::{collections::HashMap, mem};

use block::BlockContext;
use comp_type::{CompType, MaybeUnknownType};
use expression::ExpressionContext;
use writer::InstructionWriter;

use crate::{
    arch::uarch,
    error::Result,
    parser::{expression::Expression, statement::Block},
    span::Chunk,
    vm::{VirtualMachine, bytecode::OpCode},
};

pub mod block;
pub mod comp_type;
pub mod expression;
pub mod writer;

pub struct Compiler<'a> {
    variables: HashMap<&'a str, Variable<'a>>,
    variable_size: uarch,

    constants: HashMap<&'a str, Constant<'a>>,
    literals: HashMap<Literal<'a>, uarch>,
    constant_data: Vec<u8>,

    type_map: Vec<CompType<'a>>,

    instructions: InstructionWriter,
    function_data: Vec<u8>,
}

struct Variable<'a> {
    val_type: CompType<'a>,
    offset: uarch,
    initialized: bool,
}

pub struct Constant<'a> {
    pub val_type: CompType<'a>,
    pub offset: uarch,
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            variable_size: 0,
            constants: HashMap::from([(
                "u8",
                Constant {
                    val_type: CompType::Type,
                    offset: 0,
                },
            )]),
            literals: HashMap::new(),
            constant_data: vec![0, 0, 0, 0, 0, 0, 0, 0],
            type_map: vec![CompType::Number(comp_type::NumberType::U8)], // TODO: move to prelude
            instructions: InstructionWriter::default(),
            function_data: Vec::new(),
        }
    }

    pub fn compile_program(&mut self, ast: Chunk<Block<'a>>) -> Result<()> {
        self.compile_block(
            ast,
            BlockContext {
                return_type: Some(MaybeUnknownType::Infer),
                expected_tail_type: MaybeUnknownType::Infer,
                const_scope: false,
            },
        )?;
        self.instructions.data.push(OpCode::ReturnVoid as u8);
        Ok(())
    }

    pub fn create_vm(&self) -> VirtualMachine {
        VirtualMachine::new(&self.instructions.data, &self.constant_data)
    }

    fn execute_const_expr(
        &mut self,
        expr: Chunk<Expression<'a>>,
        expected_type: MaybeUnknownType<'a>,
    ) -> Result<(Box<[u8]>, CompType<'a>)> {
        let old_instructions = mem::replace(&mut self.instructions.data, Vec::new());
        let old_variables = mem::replace(&mut self.variables, HashMap::new());
        let old_variable_size = mem::replace(&mut self.variable_size, 0);

        let ty = self.compile_expr(
            expr,
            ExpressionContext {
                return_type: None,
                expected_type,
                const_scope: true,
            },
        )?;
        self.instructions.data.push(OpCode::ReturnVoid as u8);

        let mut vm = VirtualMachine::new(&self.instructions.data, &self.constant_data);
        vm.run();
        let out = vm.stack.data.into_boxed_slice();

        self.instructions.data = old_instructions;
        self.variables = old_variables;
        self.variable_size = old_variable_size;
        Ok((out, ty))
    }

    fn get_type(&self, id: Box<[u8]>) -> Result<&CompType<'a>, ()> {
        let id = uarch::from_le_bytes(*(TryInto::<Box<_>>::try_into(id).map_err(|_| ())?)) as usize;
        self.type_map.get(id).ok_or(())
    }
}

#[derive(PartialEq, Eq, Hash)]
enum Literal<'a> {
    String(&'a str),
    Boolean(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    UArch(uarch),
}

impl<'a> Literal<'a> {
    fn append_bytes_to(&self, vec: &mut Vec<u8>) {
        match self {
            Literal::String(str) => vec.extend(str.as_bytes()),
            Literal::Boolean(bool) => vec.push(*bool as u8),
            Literal::U8(n) => vec.push(*n),
            Literal::U16(n) => vec.extend(n.to_le_bytes()),
            Literal::U32(n) => vec.extend(n.to_le_bytes()),
            Literal::U64(n) => vec.extend(n.to_le_bytes()),
            Literal::UArch(n) => vec.extend(n.to_le_bytes()),
        }
    }
}

pub struct VirtualMachineFactory {
    instructions: Vec<u8>,
    constant_data: Vec<u8>,
}

impl VirtualMachineFactory {
    pub fn create(&self) -> VirtualMachine {
        VirtualMachine::new(&self.instructions, &self.constant_data)
    }
}

impl<'a> From<Compiler<'a>> for VirtualMachineFactory {
    fn from(compiler: Compiler<'a>) -> Self {
        Self {
            instructions: compiler.instructions.data,
            constant_data: compiler.constant_data,
        }
    }
}

fn min_bytes(n: uarch) -> Option<uarch> {
    if n <= u8::MAX as uarch {
        return Some(1);
    }
    if n <= u16::MAX as uarch {
        return Some(2);
    }
    #[cfg(not(feature = "16bit"))]
    {
        if n <= u32::MAX as uarch {
            return Some(4);
        }
        if n <= u64::MAX as uarch {
            return Some(8);
        }
    }

    None
}
