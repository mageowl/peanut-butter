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
                let deref = evaluate_reporter(value, state.clone())?.deref_implicit();
                state.get(up, idx).expect("to find variable").replace(deref);
            }
            Instruction::SetProp { tbl, key, value } => {
                match evaluate_reporter(tbl, state.clone())? {
                    Value::Table(tbl) => {
                        tbl[&key]
                            .replace(evaluate_reporter(value, state.clone())?.deref_implicit());
                    }
                    Value::ImplicitRef(rc) => {
                        let Value::Table(tbl) = &*rc.borrow() else {
                            panic!("type checker failed to find mismatched table type");
                        };
                        tbl[&key]
                            .replace(evaluate_reporter(value, state.clone())?.deref_implicit());
                    }
                    _ => panic!("type checker failed to find mismatched table type"),
                };
            }
            Instruction::SetRef { reference, value } => {
                let Value::Reference(reference) =
                    evaluate_reporter(reference, state.clone())?.deref_implicit()
                else {
                    panic!("type checker failed to find mismatched reference type");
                };
                reference.replace(evaluate_reporter(value, state.clone())?.deref_implicit());
            }
            Instruction::While { condition, body } => {
                let body_state = Rc::new(State::new(body.allocation, Some(state.clone())));
                loop {
                    let Value::Boolean(condition) =
                        evaluate_reporter(condition.clone(), state.clone())?.deref_implicit()
                    else {
                        panic!("type checker failed to find mismatched boolean type")
                    };
                    if !condition {
                        break;
                    }

                    evaluate_state(body.clone(), body_state.clone())?;
                }
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
        Reporter::Lambda {
            body,
            tail,
            return_ty,
            parameters,
        } => Ok(Value::Function(Rc::new(PbFunction {
            body,
            tail: tail.map(|t| *t),
            return_ty,
            parameters,
            parent: state.clone(),
        }))),
        Reporter::Get { up, idx } => Ok(Value::ImplicitRef(
            state.get(up, idx).expect("a variable").clone(),
        )),
        Reporter::RefLocal { up, idx } => {
            Ok(Value::Reference(state.get(up, idx).expect("a variable")))
        }
        Reporter::RefProp { tbl, key } => match evaluate_reporter(*tbl, state)? {
            Value::Table(tbl) => Ok(Value::Reference(tbl[&key].clone())),
            Value::ImplicitRef(rc) => {
                let Value::Table(tbl) = &*rc.borrow() else {
                    panic!("type checker failed to find mismatched table type");
                };
                Ok(Value::Reference(tbl[&key].clone()))
            }
            _ => panic!("type checker failed to find mismatched table type"),
        },
        Reporter::Deref(reporter) => {
            let reference = match evaluate_reporter(*reporter, state)? {
                Value::Reference(reference) => reference,
                Value::ImplicitRef(rc) => {
                    let Value::Reference(reference) = &*rc.borrow() else {
                        panic!("type checker failed to find mismatched ref type")
                    };
                    reference.clone()
                }
                _ => panic!("type checker failed to find mismatched ref type"),
            };
            let borrow = reference.borrow();
            Ok(borrow.clone())
        }
        Reporter::Property { tbl, key } => match evaluate_reporter(*tbl, state)? {
            Value::Table(tbl) => Ok(Value::ImplicitRef(tbl[&key].clone())),
            Value::ImplicitRef(rc) => {
                let Value::Table(tbl) = &*rc.borrow() else {
                    panic!("type checker failed to find mismatched table type");
                };
                Ok(Value::ImplicitRef(tbl[&key].clone()))
            }
            _ => panic!("type checker failed to find mismatched table type"),
        },
        Reporter::Call(reporter, vec) => {
            let args = vec
                .into_iter()
                .map(|a| evaluate_reporter(a, state.clone()))
                .collect::<Result<Vec<_>>>()?;
            match evaluate_reporter(*reporter, state)? {
                Value::Function(callable) => callable.call(args),
                Value::ImplicitRef(rc) => {
                    let Value::Function(callable) = &*rc.borrow() else {
                        panic!("type checker failed to find mismatched fn type")
                    };
                    callable.call(args)
                }
                _ => panic!("type checker failed to find mismatched fn type"),
            }
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
                let Value::Boolean(condition) =
                    evaluate_reporter(condition, state.clone())?.deref_implicit()
                else {
                    panic!("type checker failed to find mismatched boolean type")
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
        Reporter::Match { target, cases } => {
            let Some(target) = state.get(0, target) else {
                panic!("expected a variable in match expression")
            };
            for (pat, instruction_set, tail) in cases {
                if pat.matches_val(&target.borrow()) {
                    let state = Rc::new(State::new(instruction_set.allocation, Some(state)));
                    evaluate_state(instruction_set, state.clone())?;
                    return if let Some(rep) = tail {
                        evaluate_reporter(*rep, state)
                    } else {
                        Ok(Value::Unit)
                    };
                }
            }
            Ok(Value::Unit)
        }
        Reporter::Matches { target, pattern } => {
            let target = evaluate_reporter(*target, state)?;
            Ok(Value::Boolean(pattern.matches_val(&target)))
        }
        Reporter::Arithmetic { a, b, op } => {
            let Value::Number(a) = evaluate_reporter(*a, state.clone())?.deref_implicit() else {
                panic!("type check failed to find mismatched number type.");
            };
            let Value::Number(b) = evaluate_reporter(*b, state)?.deref_implicit() else {
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
            let Value::Boolean(a) = evaluate_reporter(*a, state.clone())?.deref_implicit() else {
                panic!("type check failed to find mismatched boolean type.");
            };
            let Value::Boolean(b) = evaluate_reporter(*b, state)?.deref_implicit() else {
                panic!("type check failed to find mismatched boolean type.");
            };

            Ok(Value::Boolean(if and { a && b } else { a || b }))
        }
        Reporter::Concat { a, b } => {
            let Value::String(a) = evaluate_reporter(*a, state.clone())?.deref_implicit() else {
                panic!("type check failed to find mismatched string type.");
            };
            match evaluate_reporter(*b, state)? {
                Value::String(b) => Ok(Value::String(a + &b)),
                Value::ImplicitRef(rc) => {
                    let Value::String(b) = &*rc.borrow() else {
                        panic!("type check failed to find mismatched string type.")
                    };
                    Ok(Value::String(a + b))
                }
                _ => panic!("type check failed to find mismatched string type."),
            }
        }
        Reporter::Equality { a, b } => {
            let a = evaluate_reporter(*a, state.clone())?;
            let b = evaluate_reporter(*b, state.clone())?;
            Ok(Value::Boolean(a == b))
        }
        Reporter::Inequality { a, b, op } => {
            let Value::Number(a) = evaluate_reporter(*a, state.clone())?.deref_implicit() else {
                panic!("type check failed to find mismatched number type.");
            };
            let Value::Number(b) = evaluate_reporter(*b, state)?.deref_implicit() else {
                panic!("type check failed to find mismatched number type.");
            };

            Ok(Value::Boolean(if op == Comparison::LessThan {
                a < b
            } else {
                a > b
            }))
        }
        Reporter::Negation(reporter) => {
            let Value::Number(x) = evaluate_reporter(*reporter, state)?.deref_implicit() else {
                panic!("type check failed to find mismatched number type.");
            };
            Ok(Value::Number(-x))
        }
        Reporter::BooleanNot(reporter) => {
            let Value::Boolean(x) = evaluate_reporter(*reporter, state)?.deref_implicit() else {
                panic!("type check failed to find mismatched number type.");
            };
            Ok(Value::Boolean(!x))
        }
    }
}
