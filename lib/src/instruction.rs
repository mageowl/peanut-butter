use hashbrown::HashMap;

use crate::value::{Key, Value};

pub enum Instruction {
    Set {
        up: usize,
        idx: usize,
        value: Reporter,
    },
    SetProp {
        tbl: Reporter,
        key: Key,
        value: Reporter,
    },
    Call(Reporter, Vec<Reporter>),

    Import {
        module: String,
        item: String,
        index: usize,
    },
}

pub enum Reporter {
    Const(Value),
    Table(HashMap<Key, Reporter>),
    Lambda(InstructionSet),

    RefLocal {
        up: usize,
        idx: usize,
    },
    RefProp {
        tbl: Box<Reporter>,
        key: Key,
    },

    /// Goes up the tree `up` times, then gets item `idx`.
    Get {
        up: usize,
        idx: usize,
    },
    /// Access property `key` of table `tbl`
    Property {
        tbl: Box<Reporter>,
        key: Key,
    },
    /// Call a function with arguments.
    Call(Box<Reporter>, Vec<Reporter>),
}

#[derive(Default)]
pub struct InstructionSet {
    pub allocation: usize,
    pub instructions: Vec<Instruction>,
}

impl InstructionSet {
    pub fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}
