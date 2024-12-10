use pbscript_lib::{error::Result, module_tree::ExternalModule};

pub fn build() -> ExternalModule {
    ExternalModule::builder().function("print", print).build()
}

fn print(text: String) -> Result<()> {
    println!("{}", text);
    Ok(())
}
