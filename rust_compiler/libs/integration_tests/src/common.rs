use compiler::Compiler;
use parser::Parser;
use static_analysis::Analyzer;
use tokenizer::Tokenizer;

/// Compile Slang source code and return both unoptimized and optimized output
pub fn compile_with_and_without_optimization(source: &str) -> String {
    // Compile for unoptimized output
    let tokenizer = Tokenizer::from(source);
    let parser = Parser::new(tokenizer);
    let output = parser
        .parse_all()
        .expect("Failed to parse source code")
        .expect("No parse output");

    let analyzer = Analyzer::default();
    let analyze_result = analyzer
        .analyze(&output.root)
        .expect("Failed to analyze source code");

    let compiler = Compiler::new(analyze_result, output.declaration_docs, None);
    let result = compiler.compile(output.root);

    // Get unoptimized output
    let mut unoptimized_writer = std::io::BufWriter::new(Vec::new());
    result
        .instructions
        .write(&mut unoptimized_writer)
        .expect("Failed to write unoptimized output");
    let unoptimized_bytes = unoptimized_writer
        .into_inner()
        .expect("Failed to get bytes");
    let unoptimized = String::from_utf8(unoptimized_bytes).expect("Invalid UTF-8");

    // Compile again for optimized output
    let tokenizer2 = Tokenizer::from(source);
    let parser2 = Parser::new(tokenizer2);

    let output2 = parser2
        .parse_all()
        .expect("Failed to parse source code")
        .expect("No parse output");

    let analyzer2 = Analyzer::default();
    let analyze_result2 = analyzer2
        .analyze(&output2.root)
        .expect("Failed to analyze source code");

    let compiler2 = Compiler::new(analyze_result2, output2.declaration_docs, None);
    let result2 = compiler2.compile(output2.root);

    // Apply optimizations
    let optimized_instructions = optimizer::optimize(result2.instructions);

    // Get optimized output
    let mut optimized_writer = std::io::BufWriter::new(Vec::new());
    optimized_instructions
        .write(&mut optimized_writer)
        .expect("Failed to write optimized output");
    let optimized_bytes = optimized_writer.into_inner().expect("Failed to get bytes");
    let optimized = String::from_utf8(optimized_bytes).expect("Invalid UTF-8");

    // Combine both outputs with clear separators
    format!(
        "## Unoptimized Output\n\n{}\n## Optimized Output\n\n{}",
        unoptimized, optimized
    )
}
