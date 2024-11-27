#[macro_use]
extern crate quick_error;

mod compiler;
mod parser;
mod tokenizer;

use clap::Parser;
use compiler::Compiler;
use parser::Parser as ASTParser;
use std::{
    fs::File,
    io::{BufWriter, Read, Write},
};
use tokenizer::{Tokenizer, TokenizerError};

#[macro_export]
/// A macro to create a boxed value.
macro_rules! boxed {
    ($e:expr) => {
        Box::new($e)
    };
}

quick_error! {
    #[derive(Debug)]
    enum StationlangError {
        TokenizerError(err: TokenizerError) {
            from()
            display("Tokenizer error: {}", err)
        }
        ParserError(err: parser::ParseError) {
            from()
            display("Parser error: {}", err)
        }
        CompileError(err: compiler::CompileError) {
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
    input_file: Option<String>,
    /// The stack size for the compiled program. Compilation will fail if the compiler detects that the program will exceed this stack size.
    #[arg(short, long, default_value_t = 512)]
    stack_size: usize,
    /// The output file for the compiled program. If not set, output will go to stdout.
    #[arg(short, long)]
    output_file: Option<String>,
}

fn run_logic() -> Result<(), StationlangError> {
    let args = Args::parse();
    let input_file = args.input_file;

    let tokenizer: Tokenizer = match input_file {
        Some(input_file) => Tokenizer::from_path(&input_file)?,
        None => {
            let mut buf = String::new();
            let stdin = std::io::stdin();

            let read_result = stdin.lock().read_to_string(&mut buf)?;

            if read_result == 0 {
                return Ok(());
            }

            Tokenizer::from(buf)
        }
    };

    let parser = ASTParser::new(tokenizer);

    let mut writer: BufWriter<Box<dyn Write>> = match args.output_file {
        Some(output_file) => BufWriter::new(boxed!(File::create(output_file)?)),
        None => BufWriter::new(boxed!(std::io::stdout())),
    };

    let compiler = Compiler::new(parser, args.stack_size, &mut writer);

    compiler.compile()?;

    writer.flush()?;
    Ok(())
}

fn main() {
    if let Err(e) = run_logic() {
        eprintln!("\n{}", e);
    }
}
