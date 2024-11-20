mod parser;
mod tokenizer;

use clap::Parser;
use tokenizer::{Tokenizer, TokenizerError};

#[derive(Debug, thiserror::Error)]
enum StationlangError {
    #[error("{0}")]
    TokenizerError(#[from] TokenizerError),
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// What file should be compiled
    #[arg(short, long)]
    input_file: String,
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

    let mut tokenizer = Tokenizer::from_path(&input_file)?;

    while let Some(token) = tokenizer.next_token()? {
        println!("{:?}", token);
    }

    Ok(())
}

fn main() {
    if let Err(e) = run_logic() {
        eprintln!("\n\n{}", e);
    }
}
