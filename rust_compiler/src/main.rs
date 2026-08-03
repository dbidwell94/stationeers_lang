#![allow(clippy::result_large_err)]

use clap::Parser;
use compiler::{CompilationResult, Compiler};
use parser::Parser as ASTParser;
use static_analysis::Analyzer;
use std::{
    fs::File,
    io::{stderr, BufWriter, Read, Write},
    path::PathBuf,
};
use thiserror::Error as ThisError;
use tokenizer::{self, Tokenizer};

#[derive(ThisError, Debug)]
enum CliError {
    #[error("{0}")]
    Tokenizer(String),

    #[error("{0}")]
    Parser(String),

    #[error("{0}")]
    Compile(String),

    #[error(transparent)]
    IO(#[from] std::io::Error),
}

impl From<parser::Error<'_>> for CliError {
    fn from(value: parser::Error<'_>) -> Self {
        Self::Parser(value.to_string())
    }
}

impl From<parser::Errors<'_>> for CliError {
    fn from(value: parser::Errors<'_>) -> Self {
        Self::Parser(value.to_string())
    }
}

impl From<compiler::Error<'_>> for CliError {
    fn from(value: compiler::Error<'_>) -> Self {
        Self::Compile(value.to_string())
    }
}

impl From<tokenizer::Error> for CliError {
    fn from(value: tokenizer::Error) -> Self {
        Self::Tokenizer(value.to_string())
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// What file should be compiled. If not set, input will be read from stdin.
    #[arg(short, long)]
    input_file: Option<PathBuf>,
    /// The output file for the compiled program. If not set, output will go to stdout.
    #[arg(short, long)]
    output_file: Option<PathBuf>,
    /// Should Slang attempt to optimize the output?
    #[arg(short = 'z', long)]
    optimize: bool,
}

fn run_logic() -> Result<(), CliError> {
    let args = Args::parse();
    let input_file = args.input_file;

    let input_string = match input_file {
        Some(input_path) => {
            let mut buf = String::new();
            let mut file = std::fs::File::open(input_path)?;
            file.read_to_string(&mut buf)?;
            buf
        }
        None => {
            let mut buf = String::new();
            let stdin = std::io::stdin();

            let read_result = stdin.lock().read_to_string(&mut buf)?;

            if read_result == 0 {
                return Ok(());
            }

            buf
        }
    };

    let tokenizer = Tokenizer::from(input_string.as_str());
    let parser = ASTParser::new(tokenizer);
    let output = parser.parse_all().map_err(CliError::from)?;
    let output = output.ok_or_else(|| std::io::Error::other("No parse output"))?;

    let mut writer: BufWriter<Box<dyn Write>> = match args.output_file {
        Some(output_file) => BufWriter::new(Box::new(File::create(output_file)?)),
        None => BufWriter::new(Box::new(std::io::stdout())),
    };

    let analyze_result = Analyzer::default()
        .analyze(&output.root)
        .map_err(|err| std::io::Error::other(err.to_string()))?;
    let compiler = Compiler::new(analyze_result, output.declaration_docs, None);

    let CompilationResult {
        errors,
        instructions,
        ..
    } = compiler.compile(&output.root);

    if !errors.is_empty() {
        let mut std_error = stderr();
        let errors = errors.into_iter().map(CliError::from);

        std_error.write_all(b"Compilation error:\n")?;

        for err in errors {
            std_error.write_all(format!("{}\n", err).as_bytes())?;
        }
    }

    if args.optimize {
        optimizer::optimize(instructions).write(&mut writer)?;
    } else {
        instructions.write(&mut writer)?;
    }

    writer.flush()?;

    Ok(())
}

fn main() -> anyhow::Result<()> {
    run_logic()?;

    Ok(())
}
