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

#[derive(Debug)]
pub struct Variable {
    pub value_type: Type,
    pub value: Rc<RefCell<Option<Value>>>,
    pub initialized: bool,
    pub mutable: bool,
}

#[derive(Debug, Default)]
pub struct Scope {
    pub variables: HashMap<String, Variable>,
    pub parent: Option<Rc<Scope>>,
}

#[derive(Debug)]
pub struct LocalModule {
    pub public_variables: HashMap<String, Variable>,
}

impl LocalModule {
    pub fn new() -> Self {
        Self {
            public_variables: HashMap::new(),
        }
    }
}

impl Default for LocalModule {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct ExternalModule {
    pub variables: HashMap<String, Variable>,
    pub submodules: HashMap<String, ExternalModule>,
}

impl ExternalModule {
    pub fn builder() -> ModuleBuilder {
        ModuleBuilder {
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
