#![allow(clippy::crate_in_macro_def)]

macro_rules! output {
    ($input:expr) => {
        String::from_utf8($input.into_inner()?)?
    };
}

/// Represents both compilation errors and compiled output
pub struct CompilationCheckResult {
    pub errors: Vec<crate::Error<'static>>,
    pub output: String,
}

#[cfg_attr(test, macro_export)]
macro_rules! compile {
    ($source:expr) => {{
        let owned_source = $source.to_string();
        let source = owned_source.as_str();
        let tokenizer = tokenizer::Tokenizer::from(source);
        let parser = parser::Parser::new(tokenizer);
        let mut writer = std::io::BufWriter::new(Vec::new());

        match parser.parse_all() {
            Ok(Some(output)) => {
                let analyze_result =
                    match static_analysis::Analyzer::default().analyze(&output.root) {
                        Ok(result) => result,
                        Err(_) => static_analysis::AnalyzeResult {
                            symbol_table: Default::default(),
                            functions: Default::default(),
                            documentation: Default::default(),
                        },
                    };

                let compiler =
                    crate::Compiler::new(analyze_result, output.declaration_docs.clone(), None);
                let res = compiler.compile(&output.root);
                res.instructions.write(&mut writer)?;
            }
            Ok(None) => {}
            Err(parser_errs) => {
                for e in parser_errs.0 {
                    let _ = e; // parse errors can't produce instructions
                }
            }
        }

        output!(writer)
    }};

    (result $source:expr) => {{
        let owned_source = $source.to_string();
        let source = owned_source.as_str();
        let tokenizer = tokenizer::Tokenizer::from(source);
        let parser = parser::Parser::new(tokenizer);

        match parser.parse_all() {
            Ok(Some(output)) => {
                let analyze_result =
                    match static_analysis::Analyzer::default().analyze(&output.root) {
                        Ok(result) => result,
                        Err(_) => static_analysis::AnalyzeResult {
                            symbol_table: Default::default(),
                            functions: Default::default(),
                            documentation: Default::default(),
                        },
                    };

                let compiler =
                    crate::Compiler::new(analyze_result, output.declaration_docs.clone(), None);
                let res = compiler.compile(&output.root);
                res.errors.into_iter().map(|err| err.into_owned()).collect()
            }
            Ok(None) => Vec::new(),
            Err(parser_errs) => parser_errs
                .0
                .into_iter()
                .map(|e| crate::Error::Parse(e).into_owned())
                .collect(),
        }
    }};

    (check $source:expr) => {{
        let owned_source = $source.to_string();
        let source = owned_source.as_str();
        let tokenizer = tokenizer::Tokenizer::from(source);
        let parser = parser::Parser::new(tokenizer);
        let mut writer = std::io::BufWriter::new(Vec::new());
        let errors = match parser.parse_all() {
            Ok(Some(output)) => {
                let analyze_result =
                    match static_analysis::Analyzer::default().analyze(&output.root) {
                        Ok(result) => result,
                        Err(_) => static_analysis::AnalyzeResult {
                            symbol_table: Default::default(),
                            functions: Default::default(),
                            documentation: Default::default(),
                        },
                    };

                let compiler =
                    crate::Compiler::new(analyze_result, output.declaration_docs.clone(), None);
                let res = compiler.compile(&output.root);
                res.instructions.write(&mut writer)?;
                res.errors.into_iter().map(|err| err.into_owned()).collect()
            }
            Ok(None) => Vec::new(),
            Err(parser_errs) => parser_errs
                .0
                .into_iter()
                .map(|e| crate::Error::Parse(e).into_owned())
                .collect(),
        };

        let output = output!(writer);
        crate::test::CompilationCheckResult { errors, output }
    }};

    (metadata $source:expr) => {{
        let owned_source = $source.to_string();
        let source = owned_source.as_str();
        let tokenizer = tokenizer::Tokenizer::from(source);
        let parser = parser::Parser::new(tokenizer);

        match parser.parse_all() {
            Ok(Some(output)) => {
                let analyze_result =
                    match static_analysis::Analyzer::default().analyze(&output.root) {
                        Ok(result) => result,
                        Err(_) => static_analysis::AnalyzeResult {
                            symbol_table: Default::default(),
                            functions: Default::default(),
                            documentation: Default::default(),
                        },
                    };

                let compiler =
                    crate::Compiler::new(analyze_result, output.declaration_docs.clone(), None);
                let res = compiler.compile(&output.root);
                res.metadata.into_owned()
            }
            Ok(None) => crate::CompilationMetadata::new().into_owned(),
            Err(_) => crate::CompilationMetadata::new().into_owned(),
        }
    }};
}
mod binary_expression;
mod branching;
mod declaration_function_invocation;
mod declaration_literal;
mod device_access;
mod edge_cases;
mod error_handling;
mod function_declaration;
mod logic_expression;
mod loops;
mod math_syscall;
mod negation_priority;
mod scoping;
mod symbol_documentation;
mod syscall;
mod tuple_literals;
