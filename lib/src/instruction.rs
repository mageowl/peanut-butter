use hashbrown::HashMap;

use crate::value::{Comparison, Key, Value};

#[derive(Clone)]
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
    SetRef {
        reference: Reporter,
        value: Reporter,
    },
    Void(Reporter),

    Import {
        module: String,
        item: String,
        index: usize,
    },
}

#[derive(Clone)]
pub enum Reporter {
    Const(Value),
    Table(HashMap<Key, Reporter>),
    Lambda(Box<Reporter>),

    /// Goes up the tree `up` times, then gets item `idx`.
    Get {
        up: usize,
        idx: usize,
    },
    RefLocal {
        up: usize,
        idx: usize,
    },
    RefProp {
        tbl: Box<Reporter>,
        key: Key,
    },
    Deref(Box<Reporter>),
    /// Access property `key` of table `tbl`
    Property {
        tbl: Box<Reporter>,
        key: Key,
    },
    /// Call a function with arguments.
    Call(Box<Reporter>, Vec<Reporter>),

    Block(InstructionSet),
    If {
        blocks: Vec<(Reporter, InstructionSet)>,
        else_block: Option<InstructionSet>,
    },

    Arithmetic {
        a: Box<Reporter>,
        b: Box<Reporter>,
        op: ArithmaticOperation,
    },
    BooleanOp {
        a: Box<Reporter>,
        b: Box<Reporter>,
        and: bool,
    },
    Concat {
        a: Box<Reporter>,
        b: Box<Reporter>,
    },
    Comparison {
        a: Box<Reporter>,
        b: Box<Reporter>,
        op: Comparison,
    },
    Negation(Box<Reporter>),
    BooleanNot(Box<Reporter>),
}

#[derive(Clone, Copy, Debug)]
pub enum ArithmaticOperation {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Exponentation,
}

#[derive(Default, Clone)]
pub struct InstructionSet {
    pub allocation: usize,
    pub instructions: Vec<Instruction>,
}

impl InstructionSet {
    pub fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}
