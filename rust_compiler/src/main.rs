#![allow(clippy::result_large_err)]

#[macro_use]
extern crate quick_error;

use clap::Parser;
use compiler::Compiler;
use parser::Parser as ASTParser;
use std::{
    fs::File,
    io::{stderr, BufWriter, Read, Write},
    path::PathBuf,
};
use tokenizer::{self, Tokenizer};

quick_error! {
    #[derive(Debug)]
    enum StationlangError {
        TokenizerError(err: tokenizer::Error) {
            from()
            display("Tokenizer error: {}", err)
        }
        ParserError(err: parser::Error) {
            from()
            display("Parser error: {}", err)
        }
        CompileError(err: compiler::Error) {
            from()
            display("Compile error: {}", err)
        }
        IoError(err: std::io::Error) {
            from()
            display("IO error: {}", err)
        }
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
}

fn run_logic() -> Result<(), StationlangError> {
    let args = Args::parse();
    let input_file = args.input_file;

    let input_string = match input_file {
        Some(input_path) => {
            let mut buf = String::new();
            let mut file = std::fs::File::open(input_path).unwrap();
            file.read_to_string(&mut buf).unwrap();
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

    let mut writer: BufWriter<Box<dyn Write>> = match args.output_file {
        Some(output_file) => BufWriter::new(Box::new(File::create(output_file)?)),
        None => BufWriter::new(Box::new(std::io::stdout())),
    };

    let compiler = Compiler::new(parser, &mut writer, None);

    let mut errors = compiler.compile();

    if !errors.is_empty() {
        let mut std_error = stderr();
        let last = errors.pop();
        let errors = errors.into_iter().map(StationlangError::from);

        std_error.write_all(b"Compilation error:\n")?;

        for err in errors {
            std_error.write_all(format!("{}\n", err).as_bytes())?;
        }

        return Err(StationlangError::from(last.unwrap()));
    }

    writer.flush()?;

    Ok(())
}

fn main() -> Result<(), StationlangError> {
    run_logic()?;

    Ok(())
}
