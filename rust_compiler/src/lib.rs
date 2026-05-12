#[cfg(feature = "headers")]
use std::fs;

#[cfg(feature = "headers")]
use anyhow::Result;

mod ffi;
pub(crate) mod lsp;

#[cfg(feature = "headers")]
pub fn generate_headers() -> Result<()> {
    use std::{env, path::PathBuf};

    let file_name = PathBuf::from(env::var("BINDINGS_OUT").unwrap_or("../csharp_mod".to_owned())).with_file_name("FfiGlue.cs");
    println!("{}",file_name.to_str().unwrap());
    fs::create_dir_all(&file_name.parent().unwrap() /*should not ever fail! */)?;
    ::safer_ffi::headers::builder()
        .with_language(safer_ffi::headers::Language::CSharp)
        .to_file(&file_name)?
        .generate()?;

    let content = std::fs::read_to_string(&file_name)?;

    let content = content.replace(
        "private const string RustLib = \"slang\";",
        "public const string RustLib = \"slang_compiler.dll\";",
    );

    std::fs::write(&file_name, content)?;
    Ok(())
}
