use hashbrown::HashMap;
use std::{cell::RefCell, rc::Rc};

use pbscript_lib::{
    error::{Error, Result, Warn},
    span::{Chunk, Span},
    types::Type,
    value::{Key, Value},
};

use crate::parser::{
    block::Block, expression::Expression, program::Program, statement::Statement,
    type_name::TypeName, Parameter,
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

pub struct Ancestry<'a> {
    parent: Option<&'a Ancestry<'a>>,
    scope: &'a UnlockedScope,
}

impl Ancestry<'_> {
    fn get_var(&self, name: &String) -> Option<&Variable> {
        self.scope
            .variables
            .get(name)
            .or_else(|| self.parent.map(|p| p.get_var(name))?)
    }
}

trait AncestryOption<'a> {
    fn push(&'a self, scope: &'a UnlockedScope) -> Ancestry<'a>;
}
impl<'a> AncestryOption<'a> for Option<&Ancestry<'a>> {
    fn push(&'a self, scope: &'a UnlockedScope) -> Ancestry<'a> {
        Ancestry {
            parent: *self,
            scope,
        }
    }
}

#[derive(Debug)]
pub struct Variable {
    value_type: Type,
    value: Option<Rc<RefCell<Value>>>,
    initialized: bool,
    mutable: bool,
}

#[derive(Debug)]
pub struct UnlockedScope {
    variables: HashMap<String, Variable>,
    pub warnings: Vec<Warn>,
}

#[derive(Debug)]
pub struct Scope {
    pub variables: HashMap<String, Variable>,
    pub parent: Option<Rc<Scope>>,
}

impl UnlockedScope {
    #[allow(private_bounds)]
    pub fn new(tree: &mut impl Body, parent: Option<&Ancestry<'_>>) -> Result<Self> {
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
        parent: Option<&Ancestry<'_>>,
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
        let body_type = call_scope.expression(
            body,
            Some(&Ancestry {
                parent,
                scope: &self,
            }),
        )?;

        if body_type != *return_type {
            Err(Error::new(
                return_type_span,
                "Return type does not match type of body.",
            ))
        } else {
            Ok(return_type)
        }
    }

    fn statement(
        &mut self,
        statement: &mut Chunk<Statement>,
        parent: Option<&Ancestry<'_>>,
    ) -> Result<()> {
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
        parent: Option<&Ancestry<'_>>,
    ) -> Result<Type> {
        match &mut expr.data {
            Expression::String(_) => Ok(Type::String),
            Expression::Number(_) => Ok(Type::Number),
            Expression::Boolean(_) => Ok(Type::Number),
            Expression::Table(map) => Ok(Type::Table(
                map.iter_mut()
                    .map(|(k, v)| {
                        self.expression(v.as_mut(), parent)
                            .map(|v| (k.data.clone(), v))
                    })
                    .collect::<Result<HashMap<_, _>>>()?,
            )),
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
                if let Some(var) = self.variables.get(name).or_else(|| parent?.get_var(name)) {
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
                if let Type::Table(map) = target_type {
                    if let Some(ty) = map.get(&idx.data) {
                        Ok(ty.clone())
                    } else {
                        Err(Error::new(
                            idx.span,
                            format!("Property {} does not exist on this table.", idx.data),
                        ))
                    }
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

                let mut scope = UnlockedScope::new(data, Some(&ancestry))?;
                let tail_ty = if let Some(tail) = &mut data.tail {
                    scope.expression(tail.span.with(tail.data.as_mut()), Some(&ancestry))?
                } else {
                    Type::Unit
                };

                drop(ancestry);
                *unlocked_scope = Some(scope);

                Ok(tail_ty)
            }

            _ => todo!(),
        }
    }

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
