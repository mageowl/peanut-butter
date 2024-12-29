use pbscript_lib::{error::Result, module_tree::ExternalModule};

pub fn build() -> ExternalModule {
    ExternalModule::builder()
        .function("println", println)
        .function("print", print)
        .function("num_to_str", num_to_str)
        .build()
}

fn println(text: String) -> Result<()> {
    println!("{}", text);
    Ok(())
}
fn print(text: String) -> Result<()> {
    print!("{}", text);
    Ok(())
}

fn num_to_str(x: f64) -> Result<String> {
    Ok(x.to_string())
}
