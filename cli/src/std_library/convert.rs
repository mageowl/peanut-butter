use hashbrown::HashMap;
use pbscript_lib::{
    error::Result,
    module_tree::{builder::ModuleBuilder, ExternalModule},
    types::Primitive,
    value::{Key, Value},
};
use std::fmt::Write;

pub fn build() -> ExternalModule {
    ExternalModule::builder()
        .add_function("to_str", to_str)
        .add_function("fmt_tbl", fmt_tbl)
        .build()
}

pub(super) fn add_to_prelude(builder: ModuleBuilder) -> ModuleBuilder {
    builder
        .add_function("to_str", to_str)
        .add_function("fmt_tbl", fmt_tbl)
}

fn to_str(x: Primitive) -> Result<String> {
    match x {
        Primitive::String(str) => Ok(str),
        Primitive::Number(num) => Ok(num.to_string()),
        Primitive::Boolean(bool) => Ok(bool.to_string()),
    }
}

fn fmt_tbl(tbl: HashMap<Key, Value>) -> Result<String> {
    let mut str = String::from("[\n");

    let mut i = 0;
    while tbl.contains_key(&i) {
        if i > 0 {
            str.push_str(", \n");
        }
        let _ = write!(str, "\t{}", tbl[&i]);

        i += 1;
    }

    for (k, v) in tbl.iter() {
        if let Some(key) = match k {
            Key::Named(k) => Some(k.to_string()),
            Key::Index(k) if k > &i => Some(k.to_string()),
            _ => None,
        } {
            if i > 0 {
                str.push_str(", \n");
            }
            let _ = write!(str, "\t{key} = {}", v);

            i += 1;
        }
    }
    str.push_str("\n]");

    if str.len() < 60 {
        let mut indices = Vec::new();
        for (i, c) in str.char_indices() {
            if c == '\n' || c == '\t' {
                indices.push(i);
            }
        }
        for i in indices.into_iter().rev() {
            str.remove(i);
        }
    }

    Ok(str)
}
