pub mod symbols;
#[cfg(test)]
mod test;
mod v1;
mod variable_manager;

pub use symbols::{CompilationMetadata, SymbolInfo, SymbolKind, SyscallType};
pub use v1::{CompilationResult, Compiler, CompilerConfig, Error};

pub(crate) fn compile<'a>(src: &'a str) -> anyhow::Result<CompilationResult<'a>> {
    let tokenizer = tokenizer::Tokenizer::from(src);
    let parser = parser::Parser::new(tokenizer);
    let output = parser.parse_all();

    let Ok(Some(output)) = output else {
        anyhow::bail!("Failed to parse source code: {:?}", output.err());
    };

    let analyze = static_analysis::Analyzer::default();
    let root = output.root;
    let analyze_result = analyze.analyze(&root)?;

    let compiler = Compiler::new(analyze_result, output.declaration_docs, None);
    let result = compiler.compile(&root);

    todo!()
}
