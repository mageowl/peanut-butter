use pbscript_lib::{
    error::Result,
    module_tree::{builder::ModuleBuilder, ExternalModule},
};

pub fn build() -> ExternalModule {
    ExternalModule::builder()
        .add_function("println", println)
        .add_function("print", print)
        .build()
}

pub(super) fn add_to_prelude(builder: ModuleBuilder) -> ModuleBuilder {
    builder
        .add_function("println", println)
        .add_function("print", print)
}

fn println(text: String) -> Result<()> {
    println!("{}", text);
    Ok(())
}
fn print(text: String) -> Result<()> {
    print!("{}", text);
    Ok(())
}
