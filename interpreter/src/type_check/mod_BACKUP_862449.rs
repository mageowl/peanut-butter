use hashbrown::HashMap;

use pbscript_lib::{
    error::{Error, Result, Warn},
    module_tree::{Module, ModuleTree, Variable},
    span::{Chunk, Span},
    types::Type,
    value::Key,
};

use crate::parser::{
    block::Block,
    expression::{Comparison, Expression, Operation, UnaryOperation},
    program::Program,
    statement::Statement,
    type_name::TypeName,
    Parameter,
};

mod lock;

trait Body {
    fn body(&mut self) -> &mut Vec<Chunk<Statement>>;
}

impl Body for Program {
    fn body(&mut self) -> &mut Vec<Chunk<Statement>> {
        &mut self.body
    }
}
impl Body for Block {
    fn body(&mut self) -> &mut Vec<Chunk<Statement>> {
        &mut self.body
    }
}

pub enum Ancestry<'a> {
    Scope {
        parent: &'a Ancestry<'a>,
        scope: &'a UnlockedScope,
    },
    Module {
        tree: &'a mut ModuleTree,
        module: &'a mut Module,
    },
}

impl<'a> Ancestry<'a> {
    fn get_var(&self, name: &String) -> Option<&Variable> {
        match self {
            Self::Scope { parent, scope } => {
                scope.variables.get(name).or_else(|| parent.get_var(name))
            }
            Self::Module { .. } => None,
        }
    }
    fn push(&'a self, scope: &'a UnlockedScope) -> Ancestry<'a> {
        Ancestry::Scope {
            parent: self,
            scope,
        }
    }
}

#[derive(Debug)]
pub struct UnlockedScope {
    variables: HashMap<String, Variable>,
    pub warnings: Vec<Warn>,
}

<<<<<<< HEAD
#[derive(Debug)]
pub struct Scope {
    pub variables: HashMap<String, Variable>,
    pub parent: Option<Rc<Scope>>,
}

impl UnlockedScope {
    #[allow(private_bounds)]
    pub fn new(tree: &mut impl Body, parent: Option<&Ancestry<'_>>) -> Result<Self> {
=======
impl UnlockedScope {
    pub fn new(tree: &mut impl Body, parent: &Ancestry<'_>) -> Result<Self> {
>>>>>>> 487ec14 (modules)
        let mut s = Self {
            variables: HashMap::new(),
            warnings: Vec::new(),
        };

        for item in tree.body() {
            s.statement(item, parent)?;
        }

        Ok(s)
    }

    fn fn_def(
        &mut self,
        body: Chunk<&mut Expression>,
        return_type: &Option<Chunk<TypeName>>,
        parameters: &Vec<Chunk<Parameter>>,
        parent: &Ancestry<'_>,
    ) -> Result<Box<Type>> {
        let (return_type, return_type_span) = if let Some(ty) = return_type {
            (Box::new(self.get_type(&ty.data)?), ty.span)
        } else {
            (Box::new(Type::Unit), Span::char(body.span.start))
        };

        let mut call_scope = UnlockedScope {
            variables: parameters
                .iter()
                .map(|p| {
                    Ok((
                        p.data.name.data.clone(),
                        Variable {
                            mutable: false,
                            value_type: self.get_type(&p.data.type_hint.data)?,
                            value: None,
                            initialized: true,
                        },
                    ))
                })
                .collect::<Result<HashMap<_, _>>>()?,
            warnings: Vec::new(),
        };
        let body_type = call_scope.expression(body, &parent.push(&self))?;

        if body_type != *return_type {
            Err(Error::new(
                return_type_span,
                "Return type does not match type of body.",
            ))
        } else {
            Ok(return_type)
        }
    }

    fn statement(&mut self, statement: &mut Chunk<Statement>, parent: &Ancestry<'_>) -> Result<()> {
        match &mut statement.data {
            Statement::DefVariable {
                mutable,
                name,
                type_hint,
                value,
            } => {
                let var = Variable {
                    value_type: if let Some(hint) = type_hint {
                        let ty = self.get_type(&hint.data)?;

                        if let Some(value) = value {
                            if self.expression(value.as_mut(), parent)? != ty {
                                return Err(Error::new(
                                    value.span,
                                    "Initial value of variable does not match the defined type.",
                                ));
                            }
                        }

                        ty
                    } else if let Some(value) = value {
                        self.expression(value.as_mut(), parent)?
                    } else {
                        return Err(Error::new(
                            statement.span,
                            format!(
                                "Uninitialized variables must have a defined type.\nTry adding a type: 'let {}: /* type */;'",
                                name.data
                            ),
                        ));
                    },
                    value: None,
                    initialized: value.is_some(),
                    mutable: *mutable,
                };
                self.variables.insert(name.data.clone(), var);
            }
            Statement::DefFunction {
                mutable,
                name,
                parameters,
                return_type,
                body,
            } => {
                let return_type = self.fn_def(body.as_mut(), return_type, parameters, parent)?;

                let var = Variable {
                    mutable: *mutable,
                    initialized: true,
                    value: None,
                    value_type: Type::Fn {
                        parameters: parameters
                            .iter()
                            .map(|p| self.get_type(&p.data.type_hint.data))
                            .collect::<Result<Vec<_>>>()?,
                        return_type,
                    },
                };
                self.variables.insert(name.data.clone(), var);
            }
            Statement::Expression(expr) => {
                let expr_type = self.expression(statement.span.with(expr), parent)?;
                if !Type::is_unit(&expr_type) {
                    self.warnings.push(Warn::new(statement.span, "Expression does not have unit type. Try putting it into a let binding: 'let /* variable */ = '"))
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn expression(
        &mut self,
        mut expr: Chunk<&mut Expression>,
        parent: &Ancestry<'_>,
    ) -> Result<Type> {
        match &mut expr.data {
            Expression::String(_) => Ok(Type::String),
            Expression::Number(_) => Ok(Type::Number),
            Expression::Boolean(_) => Ok(Type::Number),
            Expression::Table(map) => {
                if map.is_empty() {
                    Ok(Type::Unit)
                } else {
                    let ty_map = map
                        .iter_mut()
                        .map(|(k, v)| Ok((k.data, self.expression(v.as_mut(), parent)?)))
                        .collect::<Result<HashMap<_, _>>>()?;

                    #[allow(clippy::unwrap_used)]
                    let (_, item_ty) = ty_map.iter().next().unwrap();
                    if ty_map
                        .iter()
                        .all(|(k, v)| v == item_ty && matches!(k, Key::Index(_)))
                    {
                        Ok(Type::List(Box::new(item_ty.clone())))
                    } else {
                        Err(Error::new(expr.span, "Could not infer type of this unnamed table. Consider giving it an explicit type."))
                    }
                    /*Ok(Type::Table(
                        map.iter_mut()
                            .map(|(k, v)| {
                                self.expression(v.as_mut(), parent)
                                    .map(|v| (k.data.clone(), v))
                            })
                            .collect::<Result<HashMap<_, _>>>()?,
                    ))*/
                }
            }
            Expression::Lambda {
                parameters,
                return_type,
                body,
            } => {
                let return_type = self.fn_def(
                    body.span.with(body.data.as_mut()),
                    return_type,
                    parameters,
                    parent,
                )?;

                Ok(Type::Fn {
                    parameters: parameters
                        .iter()
                        .map(|p| self.get_type(&p.data.type_hint.data))
                        .collect::<Result<Vec<_>>>()?,
                    return_type,
                })
            }

            Expression::Variable(name) => {
                if let Some(var) = self.variables.get(name).or_else(|| parent.get_var(name)) {
                    if var.initialized {
                        Ok(var.value_type.clone())
                    } else {
                        Err(Error::new(
                            expr.span,
                            format!("Variable {name} is not initialized."),
                        ))
                    }
                } else {
                    Err(Error::new(
                        expr.span,
                        format!(
                            "Variable {name} does not exist, or is defined later in the program."
                        ),
                    ))
                }
            }
            Expression::Reference(target) => Ok(Type::Ref(Box::new(
                self.expression(expr.span.with(target.data.as_mut()), parent)?,
            ))),
            Expression::Access(target, prop) => {
                let target_type =
                    self.expression(target.span.with(target.data.as_mut()), parent)?;
                if let Type::Table(map) = target_type {
                    if let Some(ty) = map.get(&prop.data) {
                        Ok(ty.clone())
                    } else {
                        Err(Error::new(
                            prop.span,
                            format!("Property {} does not exist on this table.", prop.data),
                        ))
                    }
                } else {
                    Err(Error::new(
                        target.span,
                        "Cannot access properties of a non-table value.",
                    ))
                }
            }
            Expression::Index(target, idx) => {
                let target_type =
                    self.expression(target.span.with(target.data.as_mut()), parent)?;
<<<<<<< HEAD
                if let Type::Table(map) = target_type {
                    if let Some(ty) = map.get(&idx.data) {
=======
                if let Type::Named(_) = target_type {
                    /*if let Some(ty) = map.get(&idx.data) {
>>>>>>> 487ec14 (modules)
                        Ok(ty.clone())
                    } else {
                        Err(Error::new(
                            idx.span,
                            format!("Property {} does not exist on this table.", idx.data),
                        ))
                    }*/
                    todo!()
                } else {
                    Err(Error::new(
                        target.span,
                        "Cannot access properties of a non-table value.",
                    ))
                }
            }
            Expression::DynAccess(target, expr) => todo!(),
            Expression::Call { value, args } => {
                let fn_type = self.expression(value.span.with(value.data.as_mut()), parent)?;
                if let Type::Fn {
                    parameters,
                    return_type,
                } = fn_type
                {
                    for (param, arg) in parameters.iter().zip(args) {
                        let arg_ty = self.expression(arg.as_mut(), parent)?;
                        if arg_ty != *param {
                            return Err(Error::new(
                                arg.span,
                                format!("This argument does not match the function signature. Expected a {}, but found a {}", param.simple_name(), arg_ty.simple_name()),
                            ));
                        }
                    }

                    Ok(*return_type)
                } else {
                    Err(Error::new(
                        value.span,
                        "This is not a function, but you are trying to call it.",
                    ))
                }
            }

            Expression::Block {
                data,
                unlocked_scope,
                ..
            } => {
                let ancestry = parent.push(&self);

<<<<<<< HEAD
                let mut scope = UnlockedScope::new(data, Some(&ancestry))?;
=======
                let mut scope = UnlockedScope::new(data, &ancestry)?;
>>>>>>> 487ec14 (modules)
                let tail_ty = if let Some(tail) = &mut data.tail {
                    scope.expression(tail.span.with(tail.data.as_mut()), &ancestry)?
                } else {
                    Type::Unit
                };

                *unlocked_scope = Some(scope);

                Ok(tail_ty)
            }
            Expression::If { blocks, else_block } => {
                let mut ty = None;
                for block in blocks {
                    match self.expression(block.0.as_mut(), parent) {
                        Ok(Type::Boolean) => (),
                        Ok(_) => {
                            return Err(Error::new(
                                block.0.span,
                                "If expressions must use a boolean condition.",
                            ))
                        }
                        Err(e) => return Err(e),
                    }

                    let (tail_ty, tail_span) = if let Some(tail) = &mut block.1.data.tail {
                        (
                            self.expression(tail.span.with(tail.data.as_mut()), parent)?,
                            tail.span,
                        )
                    } else {
                        (Type::Unit, block.1.span)
                    };

                    if let Some(ref b) = ty {
                        if tail_ty != *b {
                            return Err(Error::new(tail_span, format!("Previous if conditions returned a type of {}, but this one has a different type.", b.simple_name())));
                        }
                    } else {
                        ty = Some(tail_ty);
                    }
                }

                if let Some(block) = else_block {
                    let (tail_ty, tail_span) = if let Some(tail) = &mut block.data.tail {
                        (
                            self.expression(tail.span.with(tail.data.as_mut()), parent)?,
                            tail.span,
                        )
                    } else {
                        (Type::Unit, block.span)
                    };

                    if let Some(ref b) = ty {
                        if tail_ty != *b {
                            return Err(Error::new(tail_span, format!("Previous if conditions returned a type of {}, but the else block has a different type.", b.simple_name())));
                        }
                    } else {
                        ty = Some(tail_ty);
                    }
                }

                // If conditions must have at least 1 block.
                ty.ok_or_else(|| unreachable!())
            }

            Expression::BinaryOp {
                a,
                b,
                op:
                    Chunk {
                        data:
                            Operation::Exponentation
                            | Operation::Multiplication
                            | Operation::Division
                            | Operation::Subtraction
                            | Operation::Addition,
                        ..
                    },
            } => {
                let a_ty = self.expression(a.span.with(a.data.as_mut()), parent)?;
                let b_ty = self.expression(b.span.with(b.data.as_mut()), parent)?;
                if a_ty != Type::Number {
                    return Err(Error::new(a.span, "Expected a number for arthimatic, but this is is not my brother!!! very sad. :3"));
                }
                if b_ty != Type::Number {
                    return Err(Error::new(b.span, "Expected a number for arthimatic, but this is is not my brother!!! very sad. :3"));
                }

                Ok(Type::Number)
            }
            Expression::BinaryOp {
                a,
                b,
                op:
                    Chunk {
                        data: Operation::BooleanOr | Operation::BooleanAnd,
                        ..
                    },
            } => {
                let a_ty = self.expression(a.span.with(a.data.as_mut()), parent)?;
                let b_ty = self.expression(b.span.with(b.data.as_mut()), parent)?;
                if a_ty != Type::Boolean {
                    return Err(Error::new(a.span, "Expected a boolean for logical operator, but this is is not my brother!!! very sad. :3"));
                }
                if b_ty != Type::Boolean {
                    return Err(Error::new(b.span, "Expected a boolean for logical operator, but this is is not my brother!!! very sad. :3"));
                }

                Ok(Type::Boolean)
            }
            Expression::BinaryOp {
                a,
                b,
                op:
                    Chunk {
                        data: Operation::Comparison(Comparison::Equals),
                        ..
                    },
            } => {
                let a_ty = self.expression(a.span.with(a.data.as_mut()), parent)?;
                let b_ty = self.expression(b.span.with(b.data.as_mut()), parent)?;
                if a_ty != b_ty {
                    return Err(Error::new(
                        expr.span,
                        "These two values are of different types, so they will never be equal.",
                    ));
                }

                Ok(Type::Boolean)
            }
            Expression::BinaryOp {
                a,
                b,
                op:
                    Chunk {
                        data: Operation::Comparison(_),
                        ..
                    },
            } => {
                let a_ty = self.expression(a.span.with(a.data.as_mut()), parent)?;
                let b_ty = self.expression(b.span.with(b.data.as_mut()), parent)?;
                if a_ty != Type::Number {
                    return Err(Error::new(a.span, "Expected a number for inequality (less than or greater than), but this is is not my brother!!! very sad. :3"));
                }
                if b_ty != Type::Number {
                    return Err(Error::new(b.span, "Expected a number for inequality (less than or greater than), but this is is not my brother!!! very sad. :3"));
                }

                Ok(Type::Boolean)
            }
            Expression::BinaryOp {
                op:
                    Chunk {
                        data: Operation::Access,
                        ..
                    },
                ..
            } => unreachable!(),
            Expression::UnaryOp {
                a,
                op:
                    Chunk {
                        data: UnaryOperation::Negation,
                        ..
                    },
            } => {
                let a_ty = self.expression(a.span.with(a.data.as_mut()), parent)?;
                if a_ty != Type::Number {
                    return Err(Error::new(
                        a.span,
                        "Expected a number to negate, this value is not of type number",
                    ));
                }

                Ok(Type::Number)
            }
            Expression::UnaryOp {
                a,
                op:
                    Chunk {
                        data: UnaryOperation::BooleanNot,
                        ..
                    },
            } => {
                let a_ty = self.expression(a.span.with(a.data.as_mut()), parent)?;
                if a_ty != Type::Boolean {
                    return Err(Error::new(
                        a.span,
                        "Expected a boolean to negate, this value is not of type boolean.",
                    ));
                }

                Ok(Type::Boolean)
            }
        }
    }

<<<<<<< HEAD
=======
    fn inferred_ty_expr(
        &mut self,
        expr: Chunk<&mut Expression>,
        parent: &Ancestry<'_>,
        inferred: &Type,
    ) -> Result<Type> {
        match expr.data {
            Expression::Table(map) => match inferred {
                Type::Named(path) => {
                    todo!()
                }
                _ => self.expression(expr, parent),
            },
            _ => self.expression(expr, parent),
        }
    }

>>>>>>> 487ec14 (modules)
    #[allow(clippy::only_used_in_recursion)]
    fn get_type(&self, ty: &TypeName) -> Result<Type> {
        match ty {
            TypeName::Reference(ty) => Ok(Type::Ref(Box::new(self.get_type(ty.data.as_ref())?))),
            TypeName::Named { name, generics } => match name.data.as_str() {
                "str" => match generics.len() {
                    0 => Ok(Type::String),
                    _ => Err(Error::new(
                        generics[0].span,
                        "Expected no generics for the built-in string type.",
                    )),
                },
                "num" => match generics.len() {
                    0 => Ok(Type::Number),
                    _ => Err(Error::new(
                        generics[0].span,
                        "Expected no generics for the built-in number type.",
                    )),
                },
                "bool" => match generics.len() {
                    0 => Ok(Type::Boolean),
                    _ => Err(Error::new(
                        generics[0].span,
                        "Expected no generics for the built-in boolean type.",
                    )),
                },
                "list" => match generics.len() {
                    0 => Err(Error::new(
                        name.span,
                        "Expected a generic type for the built-in list type.",
                    )),
                    1 => Ok(Type::List(Box::new(self.get_type(&generics[0].data)?))),
                    _ => Err(Error::new(
                        generics[1].span,
                        "Expected only one generic type for the built-in list type.",
                    )),
                },
                "map" => match generics.len() {
                    0 => Err(Error::new(
                        name.span,
                        "Expected a generic type for the built-in map type.",
                    )),
                    1 => Ok(Type::Map(Box::new(self.get_type(&generics[0].data)?))),
                    _ => Err(Error::new(
                        generics[1].span,
                        "Expected only one generic type for the built-in map type.",
                    )),
                },
                _ => todo!(),
            },
            TypeName::Table(map) => {
                let map = map
                    .iter()
                    .map(|(k, v)| self.get_type(&v.data).map(|v| (k.data.clone(), v)))
                    .collect::<Result<HashMap<_, _>>>()?;

                if !map.is_empty() {
                    #[allow(clippy::unwrap_used)]
                    let (_, item_ty) = map.iter().next().unwrap();
                    if map
                        .iter()
                        .all(|(k, v)| v == item_ty && matches!(k, Key::Index(_)))
                    {
                        Ok(Type::List(Box::new(item_ty.clone())))
                    } else {
                        Ok(Type::Table(map))
                    }
                } else {
                    Ok(Type::Table(map))
                }
            }
        }
    }
}
