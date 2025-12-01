mod ffi;
pub(crate) mod lsp;

#[cfg(feature = "headers")]
pub fn generate_headers() -> std::io::Result<()> {
    let file_name = "../csharp_mod/FfiGlue.cs";
    ::safer_ffi::headers::builder()
        .with_language(safer_ffi::headers::Language::CSharp)
        .to_file(file_name)?
        .generate()?;

    let content = std::fs::read_to_string(file_name)?;

    let content = content.replace(
        "private const string RustLib = \"slang\";",
        "public const string RustLib = \"slang_compiler.dll\";",
    );

    std::fs::write(file_name, content)?;
    Ok(())
}
