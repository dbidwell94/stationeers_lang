mod parser;
mod tokenizer;

use std::io::{Read, Seek};

use clap::Parser;
use parser::Parser as ASTParser;
use tokenizer::{Tokenizer, TokenizerError};

#[derive(Debug, thiserror::Error)]
enum StationlangError {
    #[error(transparent)]
    TokenizerError(#[from] TokenizerError),
    #[error(transparent)]
    ParserError(#[from] parser::ParseError),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// What file should be compiled. If not set, input will be read from stdin
    #[arg(short, long)]
    input_file: Option<String>,
    /// The default stack size for the program
    #[arg(short, long, default_value_t = 512)]
    stack_size: usize,
    /// The output file for the compiled program. If not set, output will go to stdout
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

    let mut parser = ASTParser::new(tokenizer);

    let ast = parser.parse()?;

    if let Some(ast) = ast {
        println!("{}", ast);
    }

    Ok(())
}

fn main() {
    if let Err(e) = run_logic() {
        eprintln!("\n\n{}", e);
    }
}
