use std::mem;

#[allow(unused)]
#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Pop,
    Return,
    Constant,
    LongConstant,
    StackCopy,
    StackWrite,
    PointerCopy,
    PointerWrite,
    U8Add,
    U8Subtract,
    U8Multiply,
    U8Divide,
    U16Add,
    U16Subtract,
    U16Multiply,
    U16Divide,
    U32Add,
    U32Subtract,
    U32Multiply,
    U32Divide,
    U64Add,
    U64Subtract,
    U64Multiply,
    U64Divide,
    I8Add,
    I8Subtract,
    I8Multiply,
    I8Divide,
    I16Add,
    I16Subtract,
    I16Multiply,
    I16Divide,
    I32Add,
    I32Subtract,
    I32Multiply,
    I32Divide,
    I64Add,
    I64Subtract,
    I64Multiply,
    I64Divide,
    F32Add,
    F32Subtract,
    F32Multiply,
    F32Divide,
    F64Add,
    F64Subtract,
    F64Multiply,
    F64Divide,
    Call,
    Negate,
    SysCall,
    PushZeroed,
    Property,
    Jump,
    JumpIfZero,
    JumpUnlessZero,
    Equal,
    NotEqual,
    U8LessThan,
    U8GreaterThan,
    U16LessThan,
    U16GreaterThan,
    U32LessThan,
    U32GreaterThan,
    U64LessThan,
    U64GreaterThan,
    I8LessThan,
    I8GreaterThan,
    I16LessThan,
    I16GreaterThan,
    I32LessThan,
    I32GreaterThan,
    I64LessThan,
    I64GreaterThan,
    F32LessThan,
    F32GreaterThan,
    F64LessThan,
    F64GreaterThan,

    Invalid,
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        if value > 0x4e {
            Self::Invalid
        } else {
            // SAFETY: values bigger than the maximum are handled above
            unsafe { mem::transmute(value) }
        }
    }
}
