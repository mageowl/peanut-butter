use bytecode::OpCode;

pub mod bytecode;

struct Stack<const SIZE: usize> {
    data: [u8; SIZE],
    top: usize,
}

impl<const SIZE: usize> Default for Stack<SIZE> {
    fn default() -> Self {
        Self {
            data: [0; SIZE],
            top: Default::default(),
        }
    }
}

impl<const SIZE: usize> Stack<SIZE> {
    fn push_bytes(&mut self, bytes: &[u8]) {
        assert!(self.top + bytes.len() < SIZE); // TODO: error handling
        self.data[self.top..self.top + bytes.len()].copy_from_slice(bytes);
        self.top += bytes.len();
    }
    fn push_u8(&mut self, x: u8) {
        self.data[self.top] = x;
        self.top += 1;
    }

    fn pop_u8(&mut self) -> u8 {
        self.top -= 1;
        return self.data[self.top];
    }
}

pub struct VirtualMachine<const STACK_SIZE: usize, const CONST_SIZE: usize> {
    instructions: Vec<u8>,
    ip: usize,
    constants: [u8; CONST_SIZE],
    stack: Stack<STACK_SIZE>,
    done: bool,
}

impl<const STACK_SIZE: usize, const CONST_SIZE: usize> VirtualMachine<STACK_SIZE, CONST_SIZE> {
    pub fn new(instructions: Vec<u8>, constants: [u8; CONST_SIZE]) -> Self {
        assert!(instructions.len() > 0);
        Self {
            instructions,
            ip: 0,
            constants,
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

    fn read_op_code(&mut self) -> OpCode {
        return OpCode::from(self.read_u8());
    }

    pub fn step(&mut self) {
        match self.read_op_code() {
            OpCode::Pop => {
                dbg!(self.stack.pop_u8());
            }
            OpCode::ReturnVoid => {
                self.done = true; // TODO: functions
            }
            OpCode::Constant => {
                let const_idx = self.read_u16() as usize;
                let size = self.read_u16() as usize;
                self.stack
                    .push_bytes(&self.constants[const_idx..const_idx + size]);
            }
            OpCode::LongConstant => {
                let const_idx = self.read_u32() as usize;
                let size = self.read_u16() as usize;
                self.stack
                    .push_bytes(&self.constants[const_idx..const_idx + size]);
            }
            OpCode::U8Add => {
                let b = self.stack.pop_u8();
                let a = self.stack.pop_u8();
                self.stack.push_u8(a + b);
            }
            _ => todo!(),
        }
    }

    pub fn finish(&mut self) {
        while !self.done {
            self.step();
        }
    }
}
