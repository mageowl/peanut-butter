use std::{cell::RefCell, path::PathBuf, rc::Rc};

use builder::ModuleBuilder;
use hashbrown::HashMap;

use crate::{types::Type, value::Value};

pub mod builder;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModulePath {
    Local(PathBuf),
    External(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemPath {
    pub module: ModulePath,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub value_type: Type,
    pub value: Rc<RefCell<Value>>,
    pub initialized: bool,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub value_type: Type,
    pub value: Value,
}

pub trait Item {
    fn get_type(&self) -> &Type;
    fn get_initialized(&self) -> bool;
    fn get_mutable(&self) -> bool;
    fn get(&self) -> Value;
    fn get_ref(&self) -> Option<Rc<RefCell<Value>>>;
}

impl Item for Variable {
    fn get_type(&self) -> &Type {
        &self.value_type
    }
    fn get_mutable(&self) -> bool {
        self.mutable
    }
    fn get_initialized(&self) -> bool {
        self.initialized
    }
    fn get(&self) -> Value {
        self.value.borrow().clone()
    }
    fn get_ref(&self) -> Option<Rc<RefCell<Value>>> {
        Some(self.value.clone())
    }
}
impl Item for Constant {
    fn get_type(&self) -> &Type {
        &self.value_type
    }
    fn get_mutable(&self) -> bool {
        false
    }
    fn get_initialized(&self) -> bool {
        true
    }
    fn get(&self) -> Value {
        self.value.clone()
    }
    fn get_ref(&self) -> Option<Rc<RefCell<Value>>> {
        None
    }
}

#[derive(Debug)]
pub struct Scope {
    pub imported_constants: HashMap<String, Constant>,
    pub variables: HashMap<String, Variable>,
    pub parent: Option<Rc<Scope>>,
}

impl Scope {
    pub fn get_var(&self, name: &String) -> Option<&dyn Item> {
        self.variables
            .get(name)
            .map(|x| x as &dyn Item)
            .or_else(|| self.imported_constants.get(name).map(|x| x as &dyn Item))
            .or_else(|| self.parent.as_ref().and_then(|p| p.get_var(name)))
    }
}

#[derive(Debug)]
pub struct LocalModule {
    pub public_variables: Vec<String>,
    scope: Rc<Scope>,
}

impl LocalModule {
    pub fn new(scope: Rc<Scope>) -> Self {
        Self {
            public_variables: Vec::new(),
            scope,
        }
    }
}

#[derive(Debug)]
pub struct ExternalModule {
    pub constants: HashMap<String, Constant>,
    pub variables: HashMap<String, Variable>,
    pub submodules: HashMap<String, ExternalModule>,
}

impl ExternalModule {
    pub fn builder() -> ModuleBuilder {
        ModuleBuilder {
            constants: HashMap::new(),
            variables: HashMap::new(),
            submodules: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct ModuleTree {
    local: HashMap<PathBuf, LocalModule>,
    external: HashMap<String, ExternalModule>,
}

impl ModuleTree {
    pub fn new() -> Self {
        Self {
            local: HashMap::new(),
            external: HashMap::new(),
        }
    }

    pub fn local(&self) -> &HashMap<PathBuf, LocalModule> {
        &self.local
    }
    pub fn external(&self) -> &HashMap<String, ExternalModule> {
        &self.external
    }

    pub fn insert_local(&mut self, path: PathBuf, module: LocalModule) -> bool {
        if self.local.contains_key(&path) {
            false
        } else {
            self.local.insert(path, module);
            true
        }
    }
    pub fn insert_external(&mut self, name: String, module: ExternalModule) -> bool {
        if self.external.contains_key(&name) {
            false
        } else {
            self.external.insert(name, module);
            true
        }
    }
}

impl Default for ModuleTree {
    fn default() -> Self {
        Self::new()
    }
}
