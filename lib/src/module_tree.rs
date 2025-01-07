use std::{cell::RefCell, path::PathBuf, rc::Rc};

use builder::ModuleBuilder;
use hashbrown::HashMap;

use crate::{types::Type, value::Value};

pub mod builder;

#[derive(Debug, Clone)]
pub struct ExternalVariable {
    pub value_type: Type,
    pub value: Rc<RefCell<Value>>,
    pub initialized: bool,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub struct ExternalConstant {
    pub value_type: Type,
    pub value: Rc<RefCell<Value>>,
}

#[derive(Debug)]
pub struct LocalModule {
    pub public_variables: Vec<String>,
    stack: Vec<Rc<RefCell<Value>>>,
}

impl LocalModule {
    pub fn new(stack: Vec<Rc<RefCell<Value>>>) -> Self {
        Self {
            public_variables: Vec::new(),
            stack,
        }
    }
}

#[derive(Debug)]
pub struct ExternalModule {
    pub constants: HashMap<String, ExternalConstant>,
    pub variables: HashMap<String, ExternalVariable>,
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
