use hashbrown::HashMap;

use crate::value::{Comparison, Key, Value};

#[derive(Clone, Debug)]
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
        idx: usize,
    },
}

#[derive(Clone, Debug)]
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

    Block(InstructionSet, Option<Box<Reporter>>),
    If {
        blocks: Vec<(Reporter, InstructionSet, Option<Reporter>)>,
        else_block: Option<(InstructionSet, Option<Box<Reporter>>)>,
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
    Equality {
        a: Box<Reporter>,
        b: Box<Reporter>,
    },
    Inequality {
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

#[derive(Default, Clone, Debug)]
pub struct InstructionSet {
    pub allocation: usize,
    pub instructions: Vec<Instruction>,
}

impl InstructionSet {
    pub fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}
