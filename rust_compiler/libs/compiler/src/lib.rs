#[cfg(test)]
mod test;
mod v1;
mod variable_manager;

pub use v1::{CompilationResult, Compiler, CompilerConfig, Error};
