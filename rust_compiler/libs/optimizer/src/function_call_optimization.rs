use crate::helpers::get_destination_reg;
use il::{Instruction, InstructionNode, Operand};
use std::collections::{HashMap, HashSet};

/// Analyzes the registers written by each function, ignoring compiler-generated labels.
fn analyze_clobbers(instructions: &[InstructionNode]) -> HashMap<String, HashSet<u8>> {
    let mut clobbers = HashMap::new();
    let mut current_function = None;

    for node in instructions {
        if let Instruction::LabelDef(label) = &node.instruction
            && !label.starts_with("__internal_L")
        {
            current_function = Some(label.to_string());
            clobbers
                .entry(label.to_string())
                .or_insert_with(HashSet::new);
        }

        if let Some(function) = &current_function
            && let Some(register) = get_destination_reg(&node.instruction)
            && let Some(registers) = clobbers.get_mut(function)
        {
            registers.insert(register);
        }
    }

    clobbers
}

/// Returns the number of arguments consumed by a function's register-pop prologue.
///
/// Only recognize the standard prologue below the register-argument limit. Seven
/// pops may indicate additional stack-passed arguments, so that case is ambiguous.
fn prologue_argument_count(instructions: &[InstructionNode], label: &str) -> Option<usize> {
    let label_index = instructions.iter().position(
        |node| matches!(&node.instruction, Instruction::LabelDef(name) if name.as_ref() == label),
    )?;

    let mut index = label_index + 1;
    let mut argument_count = 0;
    while index < instructions.len()
        && matches!(
            instructions[index].instruction,
            Instruction::Pop(Operand::Register(_))
        )
    {
        argument_count += 1;
        index += 1;
    }

    if argument_count == 7 {
        return None;
    }

    if matches!(
        instructions.get(index).map(|node| &node.instruction),
        Some(Instruction::Push(Operand::StackPointer))
    ) && matches!(
        instructions.get(index + 1).map(|node| &node.instruction),
        Some(Instruction::Push(Operand::ReturnAddress))
    ) {
        Some(argument_count)
    } else {
        None
    }
}

/// Removes caller saves for registers that the called function does not modify.
///
/// The compiler emits saved registers before a simple, contiguous set of argument
/// pushes, then restores those registers immediately after the call. We only
/// optimize that recognizable pattern; expressions or stack-backed arguments
/// that make the layout ambiguous are deliberately left alone.
pub fn optimize_function_calls<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let clobbers = analyze_clobbers(&input);
    let mut to_remove = HashSet::new();

    for (call_index, node) in input.iter().enumerate() {
        let Instruction::JumpAndLink(Operand::Label(target)) = &node.instruction else {
            continue;
        };
        let target = target.as_ref();
        let Some(function_clobbers) = clobbers.get(target) else {
            continue;
        };
        let Some(argument_count) = prologue_argument_count(&input, target) else {
            continue;
        };

        let mut first_push = call_index;
        while first_push > 0 && matches!(input[first_push - 1].instruction, Instruction::Push(_)) {
            first_push -= 1;
        }
        let push_count = call_index - first_push;
        if push_count <= argument_count {
            continue;
        }

        let save_count = push_count - argument_count;
        let saves = &input[first_push..first_push + save_count];
        let Some(saved_registers) = saves
            .iter()
            .map(|node| match node.instruction {
                Instruction::Push(Operand::Register(register)) => Some(register),
                _ => None,
            })
            .collect::<Option<Vec<_>>>()
        else {
            continue;
        };

        let restores = input.get(call_index + 1..call_index + 1 + save_count);
        let Some(restores) = restores else {
            continue;
        };
        let restores_match = restores.iter().zip(saved_registers.iter().rev()).all(
            |(node, register)| {
                matches!(node.instruction, Instruction::Pop(Operand::Register(restored)) if restored == *register)
            },
        );
        if !restores_match {
            continue;
        }

        for (save_offset, register) in saved_registers.iter().enumerate() {
            if !function_clobbers.contains(register) {
                to_remove.insert(first_push + save_offset);
                to_remove.insert(call_index + 1 + save_count - save_offset - 1);
            }
        }
    }

    if to_remove.is_empty() {
        return (input, false);
    }

    let output = input
        .into_iter()
        .enumerate()
        .filter_map(|(index, node)| (!to_remove.contains(&index)).then_some(node))
        .collect();
    (output, true)
}

#[cfg(test)]
mod tests {
    use super::optimize_function_calls;
    use il::{Instruction, InstructionNode, Operand};
    use std::borrow::Cow;

    fn node(instruction: Instruction<'static>) -> InstructionNode<'static> {
        InstructionNode::new(instruction, None)
    }

    fn function_label(name: &'static str) -> Instruction<'static> {
        Instruction::LabelDef(Cow::Borrowed(name))
    }

    fn simple_call(function_body: Vec<Instruction<'static>>) -> Vec<InstructionNode<'static>> {
        let mut instructions = vec![node(function_label("callee"))];
        instructions.extend(function_body.into_iter().map(node));
        instructions.extend([
            node(function_label("main")),
            node(Instruction::Push(Operand::Register(1))),
            node(Instruction::Push(Operand::Register(2))),
            node(Instruction::Push(Operand::Number(3.into()))),
            node(Instruction::JumpAndLink(Operand::Label(Cow::Borrowed(
                "callee",
            )))),
            node(Instruction::Pop(Operand::Register(1))),
            node(Instruction::Move(
                Operand::Register(1),
                Operand::Register(15),
            )),
        ]);
        instructions
    }

    #[test]
    fn removes_unmodified_caller_save_without_removing_argument_pushes() {
        let input = simple_call(vec![
            Instruction::Pop(Operand::Register(8)),
            Instruction::Pop(Operand::Register(9)),
            Instruction::Push(Operand::StackPointer),
            Instruction::Push(Operand::ReturnAddress),
            Instruction::Move(Operand::Register(15), Operand::Register(8)),
        ]);

        let (output, changed) = optimize_function_calls(input);

        assert!(changed);
        assert!(
            !output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::Register(1)))
            })
        );
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::Register(2)))
            })
        );
        assert!(output.iter().any(|node| {
            matches!(node.instruction, Instruction::Push(Operand::Number(value)) if value == 3.into())
        }));
        assert!(
            !output
                .iter()
                .any(|node| { matches!(node.instruction, Instruction::Pop(Operand::Register(1))) })
        );
    }

    #[test]
    fn retains_caller_save_when_callee_clobbers_register() {
        let input = simple_call(vec![
            Instruction::Pop(Operand::Register(8)),
            Instruction::Pop(Operand::Register(9)),
            Instruction::Push(Operand::StackPointer),
            Instruction::Push(Operand::ReturnAddress),
            Instruction::Move(Operand::Register(1), Operand::Register(8)),
        ]);

        let (output, changed) = optimize_function_calls(input);

        assert!(!changed);
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::Register(1)))
            })
        );
        assert!(
            output
                .iter()
                .any(|node| { matches!(node.instruction, Instruction::Pop(Operand::Register(1))) })
        );
    }

    #[test]
    fn tracks_clobbers_across_internal_labels() {
        let input = simple_call(vec![
            Instruction::Pop(Operand::Register(8)),
            Instruction::Pop(Operand::Register(9)),
            Instruction::Push(Operand::StackPointer),
            Instruction::Push(Operand::ReturnAddress),
            function_label("__internal_L0"),
            Instruction::Move(Operand::Register(1), Operand::Register(8)),
        ]);

        let (output, changed) = optimize_function_calls(input);

        assert!(!changed);
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::Register(1)))
            })
        );
        assert!(
            output
                .iter()
                .any(|node| { matches!(node.instruction, Instruction::Pop(Operand::Register(1))) })
        );
    }

    #[test]
    fn leaves_ambiguous_call_sequences_unchanged() {
        let mut input = simple_call(vec![
            Instruction::Pop(Operand::Register(8)),
            Instruction::Pop(Operand::Register(9)),
            Instruction::Push(Operand::StackPointer),
            Instruction::Push(Operand::ReturnAddress),
            Instruction::Move(Operand::Register(15), Operand::Register(8)),
        ]);
        let call_index = input
            .iter()
            .position(|node| matches!(node.instruction, Instruction::JumpAndLink(_)))
            .unwrap();
        input[call_index + 1] = node(Instruction::Move(
            Operand::Register(3),
            Operand::Register(1),
        ));

        let (output, changed) = optimize_function_calls(input);

        assert!(!changed);
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::Register(1)))
            })
        );
    }

    #[test]
    fn leaves_calls_with_stack_passed_arguments_unchanged() {
        let mut function_body = (8..=14)
            .map(|register| Instruction::Pop(Operand::Register(register)))
            .collect::<Vec<_>>();
        function_body.extend([
            Instruction::Push(Operand::StackPointer),
            Instruction::Push(Operand::ReturnAddress),
        ]);
        let mut input = vec![node(function_label("callee"))];
        input.extend(function_body.into_iter().map(node));
        input.extend([
            node(function_label("main")),
            node(Instruction::Push(Operand::Register(1))),
            node(Instruction::Push(Operand::Register(0))),
        ]);
        input.extend((1..=7).map(|value| node(Instruction::Push(Operand::Number(value.into())))));
        input.extend([
            node(Instruction::JumpAndLink(Operand::Label(Cow::Borrowed(
                "callee",
            )))),
            node(Instruction::Pop(Operand::Register(0))),
            node(Instruction::Pop(Operand::Register(1))),
        ]);

        let (output, changed) = optimize_function_calls(input);

        assert!(!changed);
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::Register(1)))
            })
        );
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::Register(0)))
            })
        );
        assert!(
            output
                .iter()
                .any(|node| { matches!(node.instruction, Instruction::Pop(Operand::Register(0))) })
        );
        assert!(
            output
                .iter()
                .any(|node| { matches!(node.instruction, Instruction::Pop(Operand::Register(1))) })
        );
    }
}
