use pbscript_lib::{
    error::Result,
    module_tree::{builder::ModuleBuilder, ExternalModule},
    partial_ty,
    types::partial::PartialType,
    value::function::PbFunction,
};

pub fn build() -> ExternalModule {
    ExternalModule::builder()
        .add_type("Iter", type_Iter())
        .add_function("collect", collect)
        .build()
}

pub(super) fn add_to_prelude(builder: ModuleBuilder) -> ModuleBuilder {
    builder
        .add_type("Iter", type_Iter())
        .add_function("collect", collect)
}

#[expect(non_snake_case)]
fn type_Iter() -> PartialType {
    partial_ty!(fn() -> { {generic 0} | [] })
}

// TODO: Generics
fn collect(iter: PbFunction<(), Option<f64>>) -> Result<Vec<f64>> {
    let mut vec = Vec::new();
    while let Some(next) = iter.call()? {
        vec.push(next);
    }
    Ok(vec)
}
