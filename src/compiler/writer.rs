use std::{iter, marker::PhantomData};

use crate::{
    arch::{ARCH_BYTES, iarch, uarch},
    parser::expression::{ArithmaticOperation, ComparisonOperation},
    vm::bytecode::OpCode,
};

use super::comp_type::NumberType;

#[derive(Default)]
pub(super) struct InstructionWriter {
    pub(super) data: Vec<u8>,
}

impl InstructionWriter {
    #[expect(private_bounds)]
    pub fn push(&mut self, op: impl Operation) {
        op.append_to(&mut self.data);
    }
    #[expect(private_bounds)]
    pub fn reserve<T: Operation>(&mut self) -> ReservedOperation<T> {
        let r = ReservedOperation {
            index: self.data.len(),
            phantom: PhantomData,
        };
        self.data
            .extend(iter::repeat(OpCode::Invalid as u8).take(T::size()));
        r
    }
    #[expect(private_bounds)]
    pub fn fill_reserved<T: Operation>(&mut self, op: T, slot: ReservedOperation<T>) {
        let mut slice = Vec::new(); // TODO: Better way to do this??
        op.append_to(&mut slice);
        self.data[slot.index..slot.index + T::size()].copy_from_slice(&slice);
    }
}

pub(super) struct ReservedOperation<T> {
    pub(super) index: usize,
    phantom: PhantomData<T>,
}

trait Operation {
    fn append_to(self, vec: &mut Vec<u8>);
    fn size() -> usize;
}

pub(super) struct OpConstant {
    pub(super) offset: uarch,
    pub(super) size: uarch,
}
impl Operation for OpConstant {
    fn append_to(self, vec: &mut Vec<u8>) {
        if self.offset <= u8::MAX as uarch {
            vec.extend([OpCode::Constant as u8, self.offset as u8]);
        } else {
            vec.push(OpCode::LongConstant as u8);
            vec.extend(self.offset.to_le_bytes());
        }
        vec.extend(self.size.to_le_bytes());
    }
    fn size() -> usize {
        panic!("OpConstant does not have a constant size.")
    }
}

pub(super) struct OpPop {
    pub(super) size: uarch,
}
impl Operation for OpPop {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(OpCode::Pop as u8);
        vec.extend(self.size.to_le_bytes());
    }
    fn size() -> usize {
        1 + ARCH_BYTES
    }
}

pub(super) struct OpStackCopy {
    pub(super) offset: iarch,
    pub(super) size: uarch,
}
impl Operation for OpStackCopy {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(OpCode::StackCopy as u8);
        vec.extend(self.offset.to_le_bytes());
        vec.extend(self.size.to_le_bytes());
    }
    fn size() -> usize {
        1 + ARCH_BYTES * 2
    }
}

pub(super) struct OpStackWrite {
    pub(super) offset: iarch,
    pub(super) size: uarch,
}
impl Operation for OpStackWrite {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(OpCode::StackWrite as u8);
        vec.extend(self.offset.to_le_bytes());
        vec.extend(self.size.to_le_bytes());
    }
    fn size() -> usize {
        1 + ARCH_BYTES * 2
    }
}

pub(super) struct OpPushZeroed {
    pub(super) size: uarch,
}
impl Operation for OpPushZeroed {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(OpCode::PushZeroed as u8);
        vec.extend(self.size.to_le_bytes());
    }
    fn size() -> usize {
        1 + ARCH_BYTES
    }
}

pub(super) struct OpProperty {
    pub(super) struct_size: uarch,
    pub(super) property_offset: uarch,
    pub(super) property_size: uarch,
}
impl Operation for OpProperty {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(OpCode::Property as u8);
        vec.extend(self.struct_size.to_le_bytes());
        vec.extend(self.property_offset.to_le_bytes());
        vec.extend(self.property_size.to_le_bytes());
    }
    fn size() -> usize {
        1 + ARCH_BYTES * 3
    }
}

pub struct OpEqual {
    pub(super) ty_size: uarch,
    pub(super) invert: bool,
}
impl Operation for OpEqual {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(if self.invert {
            OpCode::NotEqual
        } else {
            OpCode::Equal
        } as u8);
        vec.extend(self.ty_size.to_le_bytes());
    }
    fn size() -> usize {
        1 + ARCH_BYTES
    }
}

pub struct OpJumpIfZero {
    pub(super) distance: uarch,
    pub(super) invert: bool,
}
impl Operation for OpJumpIfZero {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(if self.invert {
            OpCode::JumpUnlessZero
        } else {
            OpCode::JumpIfZero
        } as u8);
        vec.extend(self.distance.to_le_bytes());
    }
    fn size() -> usize {
        1 + ARCH_BYTES
    }
}

pub struct OpNegate;
impl Operation for OpNegate {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(OpCode::Negate as u8);
    }
    fn size() -> usize {
        1
    }
}

pub(super) struct OpArithmatic {
    pub(super) num_ty: NumberType,
    pub(super) op: ArithmaticOperation,
}
impl Operation for OpArithmatic {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(match (self.num_ty, self.op) {
            (NumberType::U8, ArithmaticOperation::Multiply) => OpCode::U8Multiply,
            (NumberType::U8, ArithmaticOperation::Divide) => OpCode::U8Divide,
            (NumberType::U8, ArithmaticOperation::Add) => OpCode::U8Add,
            (NumberType::U8, ArithmaticOperation::Subtract) => OpCode::U8Subtract,
            (NumberType::U16, ArithmaticOperation::Multiply) => OpCode::U16Multiply,
            (NumberType::U16, ArithmaticOperation::Divide) => OpCode::U16Divide,
            (NumberType::U16, ArithmaticOperation::Add) => OpCode::U16Add,
            (NumberType::U16, ArithmaticOperation::Subtract) => OpCode::U16Subtract,
            (NumberType::U32, ArithmaticOperation::Multiply) => OpCode::U32Multiply,
            (NumberType::U32, ArithmaticOperation::Divide) => OpCode::U32Divide,
            (NumberType::U32, ArithmaticOperation::Add) => OpCode::U32Add,
            (NumberType::U32, ArithmaticOperation::Subtract) => OpCode::U32Subtract,
            (NumberType::U64, ArithmaticOperation::Multiply) => OpCode::U64Multiply,
            (NumberType::U64, ArithmaticOperation::Divide) => OpCode::U64Divide,
            (NumberType::U64, ArithmaticOperation::Add) => OpCode::U64Add,
            (NumberType::U64, ArithmaticOperation::Subtract) => OpCode::U64Subtract,
            (NumberType::I8, ArithmaticOperation::Multiply) => OpCode::I8Multiply,
            (NumberType::I8, ArithmaticOperation::Divide) => OpCode::I8Divide,
            (NumberType::I8, ArithmaticOperation::Add) => OpCode::I8Add,
            (NumberType::I8, ArithmaticOperation::Subtract) => OpCode::I8Subtract,
            (NumberType::I16, ArithmaticOperation::Multiply) => OpCode::I16Multiply,
            (NumberType::I16, ArithmaticOperation::Divide) => OpCode::I16Divide,
            (NumberType::I16, ArithmaticOperation::Add) => OpCode::I16Add,
            (NumberType::I16, ArithmaticOperation::Subtract) => OpCode::I16Subtract,
            (NumberType::I32, ArithmaticOperation::Multiply) => OpCode::I32Multiply,
            (NumberType::I32, ArithmaticOperation::Divide) => OpCode::I32Divide,
            (NumberType::I32, ArithmaticOperation::Add) => OpCode::I32Add,
            (NumberType::I32, ArithmaticOperation::Subtract) => OpCode::I32Subtract,
            (NumberType::I64, ArithmaticOperation::Multiply) => OpCode::I64Multiply,
            (NumberType::I64, ArithmaticOperation::Divide) => OpCode::I64Divide,
            (NumberType::I64, ArithmaticOperation::Add) => OpCode::I64Add,
            (NumberType::I64, ArithmaticOperation::Subtract) => OpCode::I64Subtract,
            (NumberType::F32, ArithmaticOperation::Multiply) => OpCode::F32Multiply,
            (NumberType::F32, ArithmaticOperation::Divide) => OpCode::F32Divide,
            (NumberType::F32, ArithmaticOperation::Add) => OpCode::F32Add,
            (NumberType::F32, ArithmaticOperation::Subtract) => OpCode::F32Subtract,
            (NumberType::F64, ArithmaticOperation::Multiply) => OpCode::F64Multiply,
            (NumberType::F64, ArithmaticOperation::Divide) => OpCode::F64Divide,
            (NumberType::F64, ArithmaticOperation::Add) => OpCode::F64Add,
            (NumberType::F64, ArithmaticOperation::Subtract) => OpCode::F64Subtract,
        } as u8);
    }
    fn size() -> usize {
        1
    }
}

pub(super) struct OpComparison {
    pub(super) num_ty: NumberType,
    pub(super) op: ComparisonOperation,
}
impl Operation for OpComparison {
    fn append_to(self, vec: &mut Vec<u8>) {
        vec.push(match (self.num_ty, self.op) {
            (NumberType::U8, ComparisonOperation::Lt) => OpCode::U8LessThan,
            (NumberType::U8, ComparisonOperation::LtEqual) => OpCode::U8GreaterThan,
            (NumberType::U8, ComparisonOperation::Gt) => OpCode::U8GreaterThan,
            (NumberType::U8, ComparisonOperation::GtEqual) => OpCode::U8LessThan,
            (NumberType::U16, ComparisonOperation::Lt) => OpCode::U16LessThan,
            (NumberType::U16, ComparisonOperation::LtEqual) => OpCode::U16GreaterThan,
            (NumberType::U16, ComparisonOperation::Gt) => OpCode::U16GreaterThan,
            (NumberType::U16, ComparisonOperation::GtEqual) => OpCode::U16LessThan,
            (NumberType::U32, ComparisonOperation::Lt) => OpCode::U32LessThan,
            (NumberType::U32, ComparisonOperation::LtEqual) => OpCode::U32GreaterThan,
            (NumberType::U32, ComparisonOperation::Gt) => OpCode::U32GreaterThan,
            (NumberType::U32, ComparisonOperation::GtEqual) => OpCode::U32LessThan,
            (NumberType::U64, ComparisonOperation::Lt) => OpCode::U64LessThan,
            (NumberType::U64, ComparisonOperation::LtEqual) => OpCode::U64GreaterThan,
            (NumberType::U64, ComparisonOperation::Gt) => OpCode::U64GreaterThan,
            (NumberType::U64, ComparisonOperation::GtEqual) => OpCode::U64LessThan,
            (NumberType::I8, ComparisonOperation::Lt) => OpCode::I8LessThan,
            (NumberType::I8, ComparisonOperation::LtEqual) => OpCode::I8GreaterThan,
            (NumberType::I8, ComparisonOperation::Gt) => OpCode::I8GreaterThan,
            (NumberType::I8, ComparisonOperation::GtEqual) => OpCode::I8LessThan,
            (NumberType::I16, ComparisonOperation::Lt) => OpCode::I16LessThan,
            (NumberType::I16, ComparisonOperation::LtEqual) => OpCode::I16GreaterThan,
            (NumberType::I16, ComparisonOperation::Gt) => OpCode::I16GreaterThan,
            (NumberType::I16, ComparisonOperation::GtEqual) => OpCode::I16LessThan,
            (NumberType::I32, ComparisonOperation::Lt) => OpCode::I32LessThan,
            (NumberType::I32, ComparisonOperation::LtEqual) => OpCode::I32GreaterThan,
            (NumberType::I32, ComparisonOperation::Gt) => OpCode::I32GreaterThan,
            (NumberType::I32, ComparisonOperation::GtEqual) => OpCode::I32LessThan,
            (NumberType::I64, ComparisonOperation::Lt) => OpCode::I64LessThan,
            (NumberType::I64, ComparisonOperation::LtEqual) => OpCode::I64GreaterThan,
            (NumberType::I64, ComparisonOperation::Gt) => OpCode::I64GreaterThan,
            (NumberType::I64, ComparisonOperation::GtEqual) => OpCode::I64LessThan,
            (NumberType::F32, ComparisonOperation::Lt) => OpCode::F32LessThan,
            (NumberType::F32, ComparisonOperation::LtEqual) => OpCode::F32GreaterThan,
            (NumberType::F32, ComparisonOperation::Gt) => OpCode::F32GreaterThan,
            (NumberType::F32, ComparisonOperation::GtEqual) => OpCode::F32LessThan,
            (NumberType::F64, ComparisonOperation::Lt) => OpCode::F64LessThan,
            (NumberType::F64, ComparisonOperation::LtEqual) => OpCode::F64GreaterThan,
            (NumberType::F64, ComparisonOperation::Gt) => OpCode::F64GreaterThan,
            (NumberType::F64, ComparisonOperation::GtEqual) => OpCode::F64LessThan,
        } as u8);
    }
    fn size() -> usize {
        1
    }
}
