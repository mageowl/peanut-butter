use crate::{
    arch::{iarch, uarch},
    vm::bytecode::OpCode,
};

use super::comp_type::CompType;

#[derive(Default)]
pub(super) struct InstructionWriter {
    pub(super) data: Vec<u8>,
}

impl InstructionWriter {
    pub(super) fn op_constant(&mut self, offset: uarch, ty: &CompType) {
        if offset <= u8::MAX as uarch {
            self.data.extend([OpCode::Constant as u8, offset as u8]);
        } else {
            self.data.push(OpCode::LongConstant as u8);
            self.data.extend(offset.to_le_bytes());
        }
        self.data.extend(ty.size().to_le_bytes());
    }
    pub(super) fn op_pop(&mut self, ty: &CompType) {
        self.data.push(OpCode::Pop as u8);
        self.data.extend(ty.size().to_le_bytes());
    }
    pub(super) fn op_stack_copy(&mut self, offset: iarch, ty: &CompType) {
        self.data.push(OpCode::StackCopy as u8);
        self.data.extend(offset.to_le_bytes());
        self.data.extend(ty.size().to_le_bytes());
    }
    pub(super) fn op_stack_write(&mut self, offset: iarch, ty: &CompType) {
        self.data.push(OpCode::StackWrite as u8);
        self.data.extend(offset.to_le_bytes());
        self.data.extend(ty.size().to_le_bytes());
    }
    pub(super) fn op_push_zeroed(&mut self, size: uarch) {
        self.data.push(OpCode::PushZeroed as u8);
        self.data.extend(size.to_le_bytes());
    }
    pub(super) fn op_property(&mut self, struct_size: uarch, offset: uarch, ty: &CompType) {
        self.data.push(OpCode::Property as u8);
        self.data.extend(struct_size.to_le_bytes());
        self.data.extend(offset.to_le_bytes());
        self.data.extend(ty.size().to_le_bytes());
    }
    pub(super) fn op_equal(&mut self, ty_size: uarch, invert: bool) {
        self.data.push(if invert {
            OpCode::NotEqual
        } else {
            OpCode::Equal
        } as u8);
        self.data.extend(ty_size.to_le_bytes());
    }
}
