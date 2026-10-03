#![allow(clippy::crate_in_macro_def)]

macro_rules! output {
    ($input:expr) => {
        String::from_utf8($input.into_inner()?)?
    };
}

#[test]
fn virtualized_persistent_locals_reuse_registers_and_avoid_stack_spills() {
    let mut source = String::from("device d = \"d0\";");
    for index in 0..8 {
        source.push_str(&format!(
            "let value_{index} = {index}; d.On = value_{index};"
        ));
    }

    let tokenizer = tokenizer::Tokenizer::from(source.as_str());
    let parser = parser::Parser::new(tokenizer);
    let output = parser.parse_all().unwrap().unwrap();
    let analyze_result = static_analysis::Analyzer::default()
        .analyze(&output.root)
        .unwrap();
    let compiler =
        crate::Compiler::new_virtualized_for_tests(analyze_result, output.declaration_docs);

    let result = compiler.compile(&output.root);

    assert!(result.errors.is_empty());
    let virtual_stack_pushes = result
        .instructions
        .iter()
        .filter(|node| matches!(node.instruction, il::Instruction::Push(_)))
        .count();

    let allocation = optimizer::register_allocation::allocate_registers(&result.instructions)
        .expect("virtual IL should be allocatable");
    assert_eq!(allocation.registers.len(), 8);
    assert_eq!(
        allocation
            .registers
            .values()
            .copied()
            .collect::<std::collections::HashSet<_>>()
            .len(),
        1
    );
    let allocated =
        optimizer::register_allocation::rewrite_registers(result.instructions, &allocation)
            .expect("allocated IL should contain only physical registers");
    let mut writer = std::io::BufWriter::new(Vec::new());
    ic10::write(allocated, &mut writer).expect("allocated IL should emit IC10");
    let assembly = String::from_utf8(writer.into_inner().unwrap()).unwrap();

    assert!(!assembly.contains("v0"));

    let legacy_tokenizer = tokenizer::Tokenizer::from(source.as_str());
    let legacy_parser = parser::Parser::new(legacy_tokenizer);
    let legacy_output = legacy_parser.parse_all().unwrap().unwrap();
    let legacy_analysis = static_analysis::Analyzer::default()
        .analyze(&legacy_output.root)
        .unwrap();
    let legacy_compiler =
        crate::Compiler::new(legacy_analysis, legacy_output.declaration_docs, None);
    let legacy_result = legacy_compiler.compile(&legacy_output.root);
    let legacy_stack_pushes = legacy_result
        .instructions
        .iter()
        .filter(|node| matches!(node.instruction, il::Instruction::Push(_)))
        .count();

    assert!(legacy_result.errors.is_empty());
    assert!(legacy_stack_pushes > virtual_stack_pushes);
}

#[test]
fn allocated_compilation_colors_expression_temporaries() {
    let source = r#"
        device d = "d0";
        let a = 4;
        let b = 2;
        d.On = (a + b) * 3;
    "#;
    let tokenizer = tokenizer::Tokenizer::from(source);
    let parser = parser::Parser::new(tokenizer);
    let output = parser.parse_all().unwrap().unwrap();
    let analyze_result = static_analysis::Analyzer::default()
        .analyze(&output.root)
        .unwrap();
    let raw_result = crate::Compiler::new_virtualized_for_tests(
        analyze_result.clone(),
        output.declaration_docs.clone(),
    )
    .compile(&output.root);
    assert!(raw_result.errors.is_empty());
    assert!(raw_result.instructions.iter().any(|node| {
        matches!(
            node.instruction,
            il::Instruction::Add(il::Operand::VirtualRegister(_), _, _)
        )
    }));

    let result =
        crate::Compiler::compile_allocated(analyze_result, output.declaration_docs, &output.root);
    assert!(result.errors.is_empty());
    let mut writer = std::io::BufWriter::new(Vec::new());
    ic10::write(optimizer::optimize(result.instructions), &mut writer)
        .expect("expression temporaries should be colored before IC10 emission");
}

#[test]
fn allocated_system_and_math_operations_write_virtual_destinations() {
    let source = r#"
        device furnace = "d0";
        let pressure = load(furnace, "Pressure");
        let bounded = max(pressure, 0.001);
        furnace.Setting = bounded;
    "#;
    let tokenizer = tokenizer::Tokenizer::from(source);
    let parser = parser::Parser::new(tokenizer);
    let output = parser.parse_all().unwrap().unwrap();
    let analyze_result = static_analysis::Analyzer::default()
        .analyze(&output.root)
        .unwrap();
    let raw_result = crate::Compiler::new_virtualized_for_tests(
        analyze_result.clone(),
        output.declaration_docs.clone(),
    )
    .compile(&output.root);

    assert!(raw_result.errors.is_empty());
    assert!(raw_result.instructions.iter().any(|node| {
        matches!(
            node.instruction,
            il::Instruction::Load(il::Operand::VirtualRegister(_), _, _)
        )
    }));
    assert!(raw_result.instructions.iter().any(|node| {
        matches!(
            node.instruction,
            il::Instruction::Max(il::Operand::VirtualRegister(_), _, _)
        )
    }));

    let result =
        crate::Compiler::compile_allocated(analyze_result, output.declaration_docs, &output.root);
    assert!(result.errors.is_empty());
    assert!(result.register_allocated);
    assert!(!result.instructions.iter().any(|node| {
        matches!(
            &node.instruction,
            il::Instruction::Move(_, il::Operand::Register(15))
        )
    }));

    let mut writer = std::io::BufWriter::new(Vec::new());
    ic10::write(optimizer::optimize(result.instructions), &mut writer)
        .expect("direct syscall destinations should emit valid IC10");
}

#[test]
fn allocated_assignment_coalesces_expression_result_into_existing_variable() {
    let source = r#"
        device d = "d0";
        let quantity = 10;
        let increment = 2;
        quantity = quantity + increment;
        d.On = quantity;
    "#;
    let tokenizer = tokenizer::Tokenizer::from(source);
    let parser = parser::Parser::new(tokenizer);
    let output = parser.parse_all().unwrap().unwrap();
    let analyze_result = static_analysis::Analyzer::default()
        .analyze(&output.root)
        .unwrap();
    let result =
        crate::Compiler::compile_allocated(analyze_result, output.declaration_docs, &output.root);

    assert!(result.errors.is_empty());
    assert!(result.register_allocated);
    assert!(result.instructions.iter().any(|node| {
        matches!(
            &node.instruction,
            il::Instruction::Add(
                il::Operand::Register(destination),
                il::Operand::Register(source),
                _
            ) if destination == source
        )
    }));
    assert!(!result.instructions.iter().any(|node| {
        matches!(
            &node.instruction,
            il::Instruction::Move(
                il::Operand::Register(destination),
                il::Operand::Register(source)
            ) if destination == source
        )
    }));
}

#[test]
fn allocated_compilation_retries_spilled_locals_without_leaking_virtuals() {
    let mut source = String::from("device d = \"d0\";");
    for index in 0..16 {
        source.push_str(&format!("let value_{index} = {index};"));
    }
    for index in 0..16 {
        source.push_str(&format!("d.On = value_{index};"));
    }

    let tokenizer = tokenizer::Tokenizer::from(source.as_str());
    let parser = parser::Parser::new(tokenizer);
    let output = parser.parse_all().unwrap().unwrap();
    let analyze_result = static_analysis::Analyzer::default()
        .analyze(&output.root)
        .unwrap();
    let virtualized_result = crate::Compiler::new_virtualized_for_tests(
        analyze_result.clone(),
        output.declaration_docs.clone(),
    )
    .compile(&output.root);
    assert!(virtualized_result.errors.is_empty());
    let initial_allocation =
        optimizer::register_allocation::allocate_registers(&virtualized_result.instructions)
            .unwrap();
    assert!(
        !initial_allocation.spills.is_empty(),
        "test program should exceed the available register colors"
    );
    let allocated_result = crate::Compiler::compile_allocated(
        analyze_result,
        output.declaration_docs.clone(),
        &output.root,
    );

    assert!(allocated_result.errors.is_empty());
    let allocated_stack_pushes = allocated_result
        .instructions
        .iter()
        .filter(|node| matches!(node.instruction, il::Instruction::Push(_)))
        .count();
    let mut writer = std::io::BufWriter::new(Vec::new());
    ic10::write(allocated_result.instructions, &mut writer)
        .expect("spill-retried output should contain only physical registers");

    let legacy_tokenizer = tokenizer::Tokenizer::from(source.as_str());
    let legacy_parser = parser::Parser::new(legacy_tokenizer);
    let legacy_output = legacy_parser.parse_all().unwrap().unwrap();
    let legacy_analysis = static_analysis::Analyzer::default()
        .analyze(&legacy_output.root)
        .unwrap();
    let legacy_result = crate::Compiler::new(legacy_analysis, legacy_output.declaration_docs, None)
        .compile(&legacy_output.root);
    let legacy_stack_pushes = legacy_result
        .instructions
        .iter()
        .filter(|node| matches!(node.instruction, il::Instruction::Push(_)))
        .count();

    assert!(legacy_result.errors.is_empty());
    assert!(
        allocated_stack_pushes <= legacy_stack_pushes,
        "allocator emitted {allocated_stack_pushes} stack pushes; legacy emitted {legacy_stack_pushes}"
    );
}

#[test]
fn virtualized_locals_are_saved_across_scalar_function_calls() {
    let source = r#"
        fn clobber() { let scratch = 1; return scratch; }
        device d = "d0";
        let before = 9;
        let answer = clobber();
        d.On = before;
        d.On = answer;
    "#;
    let tokenizer = tokenizer::Tokenizer::from(source);
    let parser = parser::Parser::new(tokenizer);
    let output = parser.parse_all().unwrap().unwrap();
    let analyze_result = static_analysis::Analyzer::default()
        .analyze(&output.root)
        .unwrap();
    let compiler =
        crate::Compiler::new_virtualized_for_tests(analyze_result, output.declaration_docs);

    let result = compiler.compile(&output.root);

    assert!(result.errors.is_empty());
    let call_index = result
        .instructions
        .iter()
        .position(|node| matches!(node.instruction, il::Instruction::JumpAndLink(_)))
        .expect("identity call should be emitted");
    assert!(result.instructions[..call_index].iter().any(|node| {
        matches!(
            node.instruction,
            il::Instruction::Push(il::Operand::VirtualRegister(1))
        )
    }));
    assert!(result.instructions[call_index + 1..].iter().any(|node| {
        matches!(
            node.instruction,
            il::Instruction::Pop(il::Operand::VirtualRegister(1))
        )
    }));

    let allocation = optimizer::register_allocation::allocate_registers(&result.instructions)
        .expect("call-preserved virtual IL should be allocatable");
    let allocated =
        optimizer::register_allocation::rewrite_registers(result.instructions, &allocation)
            .expect("call-preserved IL should rewrite to hardware registers");
    let caller_register = allocation.registers[&1];
    let optimized = optimizer::optimize(allocated);
    let call_index = optimized
        .iter()
        .position(|node| matches!(node.instruction, il::Instruction::JumpAndLink(_)))
        .expect("optimized clobber call should remain");
    assert!(optimized[..call_index].iter().any(|node| {
        matches!(node.instruction, il::Instruction::Push(il::Operand::Register(register)) if register == caller_register)
    }));
    assert!(optimized[call_index + 1..].iter().any(|node| {
        matches!(node.instruction, il::Instruction::Pop(il::Operand::Register(register)) if register == caller_register)
    }));
    let mut writer = std::io::BufWriter::new(Vec::new());
    ic10::write(optimized, &mut writer).expect("optimized call-preserved IL should emit IC10");
}

#[test]
fn virtualized_locals_are_restored_after_tuple_return_stack_reset() {
    let source = r#"
        fn pair() { return (1, 2); }
        device d = "d0";
        let before = 9;
        let (left, right) = pair();
        d.On = before;
        d.On = left;
        d.On = right;
    "#;
    let tokenizer = tokenizer::Tokenizer::from(source);
    let parser = parser::Parser::new(tokenizer);
    let output = parser.parse_all().unwrap().unwrap();
    let analyze_result = static_analysis::Analyzer::default()
        .analyze(&output.root)
        .unwrap();
    let compiler =
        crate::Compiler::new_virtualized_for_tests(analyze_result, output.declaration_docs);

    let result = compiler.compile(&output.root);

    assert!(result.errors.is_empty());
    let call_index = result
        .instructions
        .iter()
        .position(|node| matches!(node.instruction, il::Instruction::JumpAndLink(_)))
        .expect("tuple call should be emitted");
    let stack_restore_index = result.instructions[call_index + 1..]
        .iter()
        .position(|node| {
            matches!(
                node.instruction,
                il::Instruction::Move(il::Operand::StackPointer, il::Operand::Register(15))
            )
        })
        .map(|offset| call_index + 1 + offset)
        .expect("tuple return should restore the caller stack pointer");
    assert!(result.instructions[..call_index].iter().any(|node| {
        matches!(
            node.instruction,
            il::Instruction::Push(il::Operand::VirtualRegister(0))
        )
    }));
    assert!(
        result.instructions[stack_restore_index + 1..]
            .iter()
            .any(|node| {
                matches!(
                    node.instruction,
                    il::Instruction::Pop(il::Operand::VirtualRegister(0))
                )
            })
    );

    let allocation = optimizer::register_allocation::allocate_registers(&result.instructions)
        .expect("tuple-call virtual IL should be allocatable");
    let allocated =
        optimizer::register_allocation::rewrite_registers(result.instructions, &allocation)
            .expect("tuple-call IL should rewrite to hardware registers");
    let mut writer = std::io::BufWriter::new(Vec::new());
    ic10::write(optimizer::optimize(allocated), &mut writer)
        .expect("optimized tuple-call IL should emit IC10");
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
                            uses_arrays: false,
                        },
                    };

                let compiler =
                    crate::Compiler::new(analyze_result, output.declaration_docs.clone(), None);
                let res = compiler.compile(&output.root);
                ic10::write(res.instructions, &mut writer)?;
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
                            uses_arrays: false,
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
                            uses_arrays: false,
                        },
                    };

                let compiler =
                    crate::Compiler::new(analyze_result, output.declaration_docs.clone(), None);
                let res = compiler.compile(&output.root);
                ic10::write(res.instructions, &mut writer)?;
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
                            uses_arrays: false,
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
mod arrays;
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
