use pbscript_lib::module_tree::ExternalModule;

pub mod convert;
pub mod io;
pub mod iter;
pub mod option;
pub mod process;

pub fn prelude() -> ExternalModule {
    let mut builder = ExternalModule::builder();
    builder = convert::add_to_prelude(builder);
    builder = io::add_to_prelude(builder);
    builder = iter::add_to_prelude(builder);
    builder = option::add_to_prelude(builder);
    builder.build()
}
