use std::mem;

#[allow(unused)]
#[repr(u8)]
pub enum OpCode {
    Pop = 0x00,
    ReturnVoid = 0x01,
    ReturnValue = 0x02,
    Constant = 0x03,
    LongConstant = 0x04,
    StackCopy = 0x05,
    StackWrite = 0x06,
    PointerCopy = 0x07,
    PointerWrite = 0x08,
    U8Add = 0x09,
    U8Subtract = 0x0A,
    U8Multiply = 0x0B,
    U8Divide = 0x0C,
    U16Add = 0x0D,
    U16Subtract = 0x0E,
    U16Multiply = 0x0F,
    U16Divide = 0x10,
    U32Add = 0x11,
    U32Subtract = 0x12,
    U32Multiply = 0x13,
    U32Divide = 0x14,
    U64Add = 0x15,
    U64Subtract = 0x16,
    U64Multiply = 0x17,
    U64Divide = 0x18,
    I8Add = 0x19,
    I8Subtract = 0x1A,
    I8Multiply = 0x1B,
    I8Divide = 0x1C,
    I16Add = 0x1D,
    I16Subtract = 0x1E,
    I16Multiply = 0x1F,
    I16Divide = 0x20,
    I32Add = 0x21,
    I32Subtract = 0x22,
    I32Multiply = 0x23,
    I32Divide = 0x24,
    I64Add = 0x25,
    I64Subtract = 0x26,
    I64Multiply = 0x27,
    I64Divide = 0x28,
    F32Add = 0x29,
    F32Subtract = 0x2A,
    F32Multiply = 0x2B,
    F32Divide = 0x2C,
    F64Add = 0x2D,
    F64Subtract = 0x2E,
    F64Multiply = 0x2F,
    F64Divide = 0x30,

    Invalid,
}

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        if value > 35 {
            Self::Invalid
        } else {
            // SAFETY: values bigger than the maximum are handled above
            unsafe { mem::transmute(value) }
        }
    }
}
