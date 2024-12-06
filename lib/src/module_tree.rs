use std::{cell::RefCell, rc::Rc};

use hashbrown::HashMap;

use crate::{types::Type, value::Value};

pub type ModulePath = Vec<String>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemPath {
    pub module: ModulePath,
    pub name: String,
}

#[derive(Debug)]
pub struct Variable {
    pub value_type: Type,
    pub value: Option<Rc<RefCell<Value>>>,
    pub initialized: bool,
    pub mutable: bool,
}

#[derive(Debug, Default)]
pub struct Scope {
    pub variables: HashMap<String, Variable>,
    pub parent: Option<Rc<Scope>>,
}

#[derive(Debug)]
pub struct Module {
    pub public: Scope,
    pub path: ModulePath,
    pub submodules: HashMap<String, Module>,
}

impl Module {
    pub fn new(path: ModulePath) -> Self {
        Self {
            public: Scope::default(),
            path,
            submodules: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct ModuleTree {
    pub loaded: HashMap<String, Module>,
}

impl ModuleTree {
    pub fn new() -> Self {
        Self {
            loaded: HashMap::new(),
        }
    }
}

impl Default for ModuleTree {
    fn default() -> Self {
        Self::new()
    }
}
