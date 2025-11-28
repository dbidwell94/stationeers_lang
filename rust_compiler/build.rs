fn main() -> ::std::io::Result<()> {
    safer_ffi::headers::builder()
        .with_language(safer_ffi::headers::Language::CSharp)
        .to_file("../csharp_mod/SlangStubs.cs")?
        .generate()
        .unwrap();

    Ok(())
}
