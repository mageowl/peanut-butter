use pbscript_lib::{
    module_tree::{builder::ModuleBuilder, ExternalModule},
    types::{GenericType, IntoType},
};

pub fn build() -> ExternalModule {
    ExternalModule::builder()
        .add_type("Option", PbOption::into_type())
        .build()
}

pub(super) fn add_to_prelude(builder: ModuleBuilder) -> ModuleBuilder {
    builder.add_type("Option", PbOption::into_type())
}

type PbOption = Option<GenericType<0>>;
