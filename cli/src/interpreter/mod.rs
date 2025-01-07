use std::{cell::RefCell, iter, rc::Rc};

use function::PbFunction;
use pbscript_lib::{
    error::Result,
    instruction::{ArithmaticOperation, Instruction, InstructionSet, Reporter},
    value::{Comparison, Value},
};

mod function;

type Stack = Vec<Rc<RefCell<Value>>>;
pub struct State {
    pub(crate) stack: Stack,
    pub(crate) parent: Option<Rc<State>>,
}
impl State {
    fn new(alloc: usize, parent: Option<Rc<State>>) -> Self {
        Self {
            stack: iter::repeat_with(|| Rc::new(RefCell::new(Value::Unit)))
                .take(alloc)
                .collect(),
            parent,
        }
    }

    fn get(&self, up: usize, idx: usize) -> Option<Rc<RefCell<Value>>> {
        match up {
            0 => self.stack.get(idx).cloned(),
            1.. => self.parent.as_ref().and_then(|p| p.get(up - 1, idx)),
        }
    }
}

pub fn evaluate(instructions: InstructionSet, parent: Option<Rc<State>>) -> Result<Stack> {
    let state = Rc::new(State::new(instructions.allocation, parent));
    evaluate_state(instructions, state.clone())?;
    Ok(state.stack.clone())
}

fn evaluate_state(instruction_set: InstructionSet, state: Rc<State>) -> Result<()> {
    for inst in instruction_set.instructions {
        match inst {
            Instruction::Set { up, idx, value } => {
                state
                    .get(up, idx)
                    .expect("to find variable")
                    .replace(evaluate_reporter(value, state.clone())?);
            }
            Instruction::SetProp { tbl, key, value } => {
                let Value::Table(tbl) = evaluate_reporter(tbl, state.clone())? else {
                    panic!("type checker failed to find mismatched table type");
                };
                tbl[&key].replace(evaluate_reporter(value, state.clone())?);
            }
            Instruction::SetRef { reference, value } => {
                let Value::Reference(reference) = evaluate_reporter(reference, state.clone())?
                else {
                    panic!("type checker failed to find mismatched reference type");
                };
                reference.replace(evaluate_reporter(value, state.clone())?);
            }
            Instruction::Void(reporter) => {
                evaluate_reporter(reporter, state.clone())?;
            }
            Instruction::Import { module, item, idx } => todo!(),
        }
    }

    Ok(())
}

fn evaluate_reporter(reporter: Reporter, state: Rc<State>) -> Result<Value> {
    match reporter {
        Reporter::Const(value) => Ok(value),
        Reporter::Table(hash_map) => Ok(Value::Table(
            hash_map
                .into_iter()
                .map(|(k, v)| {
                    Ok((
                        k,
                        Rc::new(RefCell::new(evaluate_reporter(v, state.clone())?)),
                    ))
                })
                .collect::<Result<_>>()?,
        )),
        Reporter::Lambda(reporter) => Ok(Value::Function(Rc::new(PbFunction {
            reporter: *reporter,
            signature: String::new(),
            parent: state.clone(),
        }))),
        Reporter::Get { up, idx } => Ok(state.get(up, idx).expect("a variable").borrow().clone()),
        Reporter::RefLocal { up, idx } => {
            Ok(Value::Reference(state.get(up, idx).expect("a variable")))
        }
        Reporter::RefProp { tbl, key } => {
            let Value::Table(tbl) = evaluate_reporter(*tbl, state)? else {
                panic!("type checker failed to find mismatched table type");
            };
            Ok(Value::Reference(tbl[&key].clone()))
        }
        Reporter::Deref(reporter) => {
            let Value::Reference(reference) = evaluate_reporter(*reporter, state)? else {
                panic!("type checker failed to find mismatched ref type");
            };
            let borrow = reference.borrow();
            Ok(borrow.clone())
        }
        Reporter::Property { tbl, key } => {
            let Value::Table(tbl) = evaluate_reporter(*tbl, state)? else {
                panic!("type checker failed to find mismatched table type");
            };
            let borrow = tbl[&key].borrow();
            Ok(borrow.clone())
        }
        Reporter::Call(reporter, vec) => {
            let args = vec
                .into_iter()
                .map(|a| evaluate_reporter(a, state.clone()))
                .collect::<Result<Vec<_>>>()?;
            let Value::Function(callable) = evaluate_reporter(*reporter, state)? else {
                panic!("type checker failed to find mismatched table type");
            };
            callable.call(args)
        }
        Reporter::Block(instruction_set, reporter) => {
            let state = Rc::new(State::new(instruction_set.allocation, Some(state)));
            evaluate_state(instruction_set, state.clone())?;
            if let Some(rep) = reporter {
                evaluate_reporter(*rep, state)
            } else {
                Ok(Value::Unit)
            }
        }
        Reporter::If { blocks, else_block } => {
            for (condition, instruction_set, tail) in blocks {
                let Value::Boolean(condition) = evaluate_reporter(condition, state.clone())? else {
                    panic!("type checker failed to find mismatched boolean type");
                };
                if condition {
                    let state = Rc::new(State::new(instruction_set.allocation, Some(state)));
                    evaluate_state(instruction_set, state.clone())?;
                    if let Some(rep) = tail {
                        return evaluate_reporter(rep, state);
                    } else {
                        return Ok(Value::Unit);
                    }
                }
            }

            if let Some((instruction_set, tail)) = else_block {
                let state = Rc::new(State::new(instruction_set.allocation, Some(state)));
                evaluate_state(instruction_set, state.clone())?;
                if let Some(rep) = tail {
                    evaluate_reporter(*rep, state)
                } else {
                    Ok(Value::Unit)
                }
            } else {
                Ok(Value::Unit)
            }
        }
        Reporter::Arithmetic { a, b, op } => {
            let Value::Number(a) = evaluate_reporter(*a, state.clone())? else {
                panic!("type check failed to find mismatched number type.");
            };
            let Value::Number(b) = evaluate_reporter(*b, state)? else {
                panic!("type check failed to find mismatched number type.");
            };

            Ok(Value::Number(match op {
                ArithmaticOperation::Addition => a + b,
                ArithmaticOperation::Subtraction => a - b,
                ArithmaticOperation::Multiplication => a * b,
                ArithmaticOperation::Division => a / b,
                ArithmaticOperation::Exponentation => {
                    if b.round() == b {
                        a.powi(b as i32)
                    } else {
                        a.powf(b)
                    }
                }
            }))
        }
        Reporter::BooleanOp { a, b, and } => {
            let Value::Boolean(a) = evaluate_reporter(*a, state.clone())? else {
                panic!("type check failed to find mismatched boolean type.");
            };
            let Value::Boolean(b) = evaluate_reporter(*b, state)? else {
                panic!("type check failed to find mismatched boolean type.");
            };

            Ok(Value::Boolean(if and { a && b } else { a || b }))
        }
        Reporter::Concat { a, b } => {
            let Value::String(a) = evaluate_reporter(*a, state.clone())? else {
                panic!("type check failed to find mismatched string type.");
            };
            let Value::String(b) = evaluate_reporter(*b, state)? else {
                panic!("type check failed to find mismatched string type.");
            };

            Ok(Value::String(a + &b))
        }
        Reporter::Equality { a, b } => {
            let a = evaluate_reporter(*a, state.clone())?;
            let b = evaluate_reporter(*b, state.clone())?;
            Ok(Value::Boolean(a == b))
        }
        Reporter::Inequality { a, b, op } => {
            let Value::Number(a) = evaluate_reporter(*a, state.clone())? else {
                panic!("type check failed to find mismatched number type.");
            };
            let Value::Number(b) = evaluate_reporter(*b, state)? else {
                panic!("type check failed to find mismatched number type.");
            };

            Ok(Value::Boolean(if op == Comparison::LessThan {
                a < b
            } else {
                a > b
            }))
        }
        Reporter::Negation(reporter) => {
            let Value::Number(x) = evaluate_reporter(*reporter, state)? else {
                panic!("type check failed to find mismatched number type.");
            };
            Ok(Value::Number(-x))
        }
        Reporter::BooleanNot(reporter) => {
            let Value::Boolean(x) = evaluate_reporter(*reporter, state)? else {
                panic!("type check failed to find mismatched number type.");
            };
            Ok(Value::Boolean(!x))
        }
    }
}
