use std::iter;

use bytecode::OpCode;

use crate::{arch::iarch, arch::uarch};

pub mod bytecode;

#[derive(Default)]
pub(crate) struct Stack {
    pub(crate) data: Vec<u8>,
}

impl Stack {
    fn push_bytes(&mut self, bytes: &[u8]) {
        self.data.extend_from_slice(bytes);
    }
    fn push_u8(&mut self, x: u8) {
        self.data.push(x)
    }
    fn push_u16(&mut self, x: u16) {
        self.data.extend(x.to_le_bytes());
    }
    fn push_u32(&mut self, x: u32) {
        self.data.extend(x.to_le_bytes());
    }
    fn push_u64(&mut self, x: u64) {
        self.data.extend(x.to_le_bytes());
    }

    fn pop_u8(&mut self) -> u8 {
        return self.data.pop().unwrap(); // TODO: error handling
    }
    fn pop_u16(&mut self) -> u16 {
        return u16::from_be_bytes([self.data.pop().unwrap(), self.data.pop().unwrap()]); // big endian because the bytes are given backward.
    }
    fn pop_u32(&mut self) -> u32 {
        return u32::from_be_bytes([
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
        ]);
    }
    fn pop_u64(&mut self) -> u64 {
        return u64::from_be_bytes([
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
            self.data.pop().unwrap(),
        ]);
    }
    fn pop_bytes(&mut self, n: usize) -> Vec<u8> {
        let new_len = self.data.len() - n;
        self.data.split_off(new_len)
    }
}

pub struct VirtualMachine<'f> {
    pub(crate) instructions: &'f Vec<u8>,
    pub(crate) ip: usize,
    pub(crate) call_stack: Vec<usize>,
    pub(crate) constant_data: &'f Vec<u8>,
    pub(crate) stack: Stack,
    pub(crate) done: bool,
}

impl<'f> VirtualMachine<'f> {
    pub fn new(instructions: &'f Vec<u8>, constant_data: &'f Vec<u8>) -> Self {
        assert!(instructions.len() > 0);
        Self {
            instructions,
            ip: 0,
            call_stack: Vec::new(),
            constant_data,
            stack: Stack::default(),
            done: false,
        }
    }

    fn read_u8(&mut self) -> u8 {
        self.ip += 1;
        return self.instructions[self.ip - 1];
    }
    fn read_u16(&mut self) -> u16 {
        self.ip += 2;
        return u16::from_le_bytes([
            self.instructions[self.ip - 2],
            self.instructions[self.ip - 1],
        ]);
    }
    fn read_u32(&mut self) -> u32 {
        self.ip += 4;
        return u32::from_le_bytes([
            self.instructions[self.ip - 4],
            self.instructions[self.ip - 3],
            self.instructions[self.ip - 2],
            self.instructions[self.ip - 1],
        ]);
    }
    fn read_u64(&mut self) -> u64 {
        self.ip += 8;
        return u64::from_le_bytes([
            self.instructions[self.ip - 8],
            self.instructions[self.ip - 7],
            self.instructions[self.ip - 6],
            self.instructions[self.ip - 5],
            self.instructions[self.ip - 4],
            self.instructions[self.ip - 3],
            self.instructions[self.ip - 2],
            self.instructions[self.ip - 1],
        ]);
    }
    fn read_i8(&mut self) -> i8 {
        self.ip += 1;
        return self.instructions[self.ip - 1] as i8;
    }
    fn read_i16(&mut self) -> i16 {
        self.ip += 2;
        return i16::from_le_bytes([
            self.instructions[self.ip - 2],
            self.instructions[self.ip - 1],
        ]);
    }
    fn read_i32(&mut self) -> i32 {
        self.ip += 4;
        return i32::from_le_bytes([
            self.instructions[self.ip - 4],
            self.instructions[self.ip - 3],
            self.instructions[self.ip - 2],
            self.instructions[self.ip - 1],
        ]);
    }
    fn read_i64(&mut self) -> i64 {
        self.ip += 8;
        return i64::from_le_bytes([
            self.instructions[self.ip - 8],
            self.instructions[self.ip - 7],
            self.instructions[self.ip - 6],
            self.instructions[self.ip - 5],
            self.instructions[self.ip - 4],
            self.instructions[self.ip - 3],
            self.instructions[self.ip - 2],
            self.instructions[self.ip - 1],
        ]);
    }
    fn read_uarch(&mut self) -> uarch {
        #[cfg(feature = "16bit")]
        return self.read_u16();
        #[cfg(not(feature = "16bit"))]
        return self.read_u64();
    }
    fn read_iarch(&mut self) -> iarch {
        #[cfg(feature = "16bit")]
        return self.read_i16();
        #[cfg(not(feature = "16bit"))]
        return self.read_i64();
    }

    fn read_op_code(&mut self) -> OpCode {
        return OpCode::from(self.read_u8());
    }

    pub fn step(&mut self) {
        match self.read_op_code() {
            OpCode::Pop => {
                let size = self.read_uarch() as usize;
                dbg!(self.stack.pop_bytes(size));
            }
            OpCode::ReturnVoid => {
                self.done = true; // TODO: functions
            }
            OpCode::Constant => {
                let offset = self.read_u8() as usize;
                let size = self.read_uarch() as usize;
                self.stack
                    .push_bytes(&self.constant_data[offset..offset + size]);
            }
            OpCode::LongConstant => {
                let offset = self.read_uarch() as usize;
                let size = self.read_uarch() as usize;
                self.stack
                    .push_bytes(&self.constant_data[offset..offset + size]);
            }
            OpCode::StackCopy => {
                let offset = self.read_iarch() as isize;
                let idx = if offset < 0 {
                    self.stack.data.len() - (-offset) as usize
                } else {
                    offset as usize
                };
                let size = self.read_uarch() as usize;
                self.stack.data.extend_from_within(idx..idx + size);
            }
            OpCode::StackWrite => {
                let offset = self.read_iarch() as isize;
                let size = self.read_uarch() as usize;
                let len = self.stack.data.len() - size;
                let idx = if offset < 0 {
                    self.stack.data.len() - (-offset) as usize
                } else {
                    offset as usize
                };
                self.stack.data.copy_within(len.., idx);
                self.stack.data.truncate(len);
            }
            OpCode::U8Add => {
                let b = self.stack.pop_u8();
                let a = self.stack.pop_u8();
                self.stack.push_u8(a + b);
            }
            OpCode::PushZeroed => {
                let size = self.read_uarch() as usize;
                self.stack.data.resize(self.stack.data.len() + size, 0);
            }
            OpCode::Property => {
                let struct_size = self.read_uarch() as usize;
                let offset = self.read_uarch() as usize;
                let size = self.read_uarch() as usize;

                let idx = self.stack.data.len() - struct_size + offset;
                let src = self.stack.data[idx..idx + size].to_vec();

                self.stack
                    .data
                    .truncate(self.stack.data.len() - struct_size + size);
                let start = self.stack.data.len() - size;
                self.stack.data[start..].copy_from_slice(&src);
            }
            op_code => {
                dbg!(op_code);
                todo!()
            }
        }
    }

    pub fn run(&mut self) {
        while !self.done {
            self.step();
        }
    }
}
