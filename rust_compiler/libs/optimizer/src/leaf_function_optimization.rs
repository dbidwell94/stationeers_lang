use crate::leaf_function::find_leaf_functions;
use il::{Instruction, InstructionNode, Operand};
use std::collections::HashSet;

fn is_internal_label(instruction: &Instruction) -> bool {
    matches!(instruction, Instruction::LabelDef(label) if label.starts_with("__internal_L"))
}

fn is_register_pop(instruction: &Instruction) -> bool {
    matches!(instruction, Instruction::Pop(Operand::Register(_)))
}

fn uses_stack(instruction: &Instruction) -> bool {
    if matches!(
        instruction,
        Instruction::Push(_) | Instruction::Pop(_) | Instruction::Peek(_)
    ) {
        return true;
    }

    let mut uses_stack_pointer = false;
    instruction.visit_operands(|operand| {
        uses_stack_pointer |= matches!(operand, Operand::StackPointer);
    });
    uses_stack_pointer
}

/// Removes the saved stack pointer and return address from simple leaf functions.
///
/// A function is eligible only when it has the compiler's standard prologue and
/// epilogue, and its body does not use the stack. Stack-using functions retain
/// their frame because local spills and tuple returns depend on its layout.
pub fn optimize_leaf_functions<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let leaves = find_leaf_functions(&input);
    if leaves.is_empty() {
        return (input, false);
    }

    let mut to_remove = HashSet::new();
    let mut function_start = None;

    for (index, node) in input.iter().enumerate() {
        if matches!(node.instruction, Instruction::LabelDef(_))
            && !is_internal_label(&node.instruction)
        {
            if let Some(start) = function_start.take() {
                optimize_function_frame(&input, start, index, &leaves, &mut to_remove);
            }
            if matches!(node.instruction, Instruction::LabelDef(_)) {
                function_start = Some(index);
            }
        }
    }

    if let Some(start) = function_start {
        optimize_function_frame(&input, start, input.len(), &leaves, &mut to_remove);
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

fn optimize_function_frame(
    instructions: &[InstructionNode],
    start: usize,
    end: usize,
    leaves: &HashSet<String>,
    to_remove: &mut HashSet<usize>,
) {
    let Instruction::LabelDef(name) = &instructions[start].instruction else {
        return;
    };
    if !leaves.contains(name.as_ref()) || end.saturating_sub(start) < 5 {
        return;
    }

    let mut prologue_start = start + 1;
    while prologue_start < end && is_register_pop(&instructions[prologue_start].instruction) {
        prologue_start += 1;
    }

    if prologue_start + 1 >= end
        || !matches!(
            instructions[prologue_start].instruction,
            Instruction::Push(Operand::StackPointer)
        )
        || !matches!(
            instructions[prologue_start + 1].instruction,
            Instruction::Push(Operand::ReturnAddress)
        )
    {
        return;
    }

    if end < 3
        || !matches!(
            instructions[end - 3].instruction,
            Instruction::Pop(Operand::ReturnAddress)
        )
        || !matches!(
            instructions[end - 2].instruction,
            Instruction::Pop(Operand::StackPointer)
        )
        || !matches!(
            instructions[end - 1].instruction,
            Instruction::Jump(Operand::ReturnAddress)
        )
    {
        return;
    }

    let body_start = prologue_start + 2;
    let epilogue_start = end - 3;
    if instructions[body_start..epilogue_start]
        .iter()
        .any(|node| uses_stack(&node.instruction))
    {
        return;
    }

    to_remove.insert(prologue_start);
    to_remove.insert(prologue_start + 1);
    to_remove.insert(epilogue_start);
    to_remove.insert(epilogue_start + 1);
}

#[cfg(test)]
mod tests {
    use super::optimize_leaf_functions;
    use il::{Instruction, InstructionNode, Operand};
    use std::borrow::Cow;

    fn node(instruction: Instruction<'static>) -> InstructionNode<'static> {
        InstructionNode::new(instruction, None)
    }

    fn label(name: &'static str) -> Instruction<'static> {
        Instruction::LabelDef(Cow::Borrowed(name))
    }

    fn simple_leaf(body: Vec<Instruction<'static>>) -> Vec<InstructionNode<'static>> {
        let mut instructions = vec![
            node(label("small")),
            node(Instruction::Pop(Operand::Register(8))),
            node(Instruction::Push(Operand::StackPointer)),
            node(Instruction::Push(Operand::ReturnAddress)),
        ];
        instructions.extend(body.into_iter().map(node));
        instructions.extend([
            node(label("__internal_L0")),
            node(Instruction::Pop(Operand::ReturnAddress)),
            node(Instruction::Pop(Operand::StackPointer)),
            node(Instruction::Jump(Operand::ReturnAddress)),
            node(label("main")),
        ]);
        instructions
    }

    #[test]
    fn removes_frame_from_stack_free_leaf_function() {
        let input = simple_leaf(vec![Instruction::Add(
            Operand::Register(1),
            Operand::Register(8),
            Operand::Number(1.into()),
        )]);
        assert!(super::find_leaf_functions(&input).contains("small"));

        let (output, changed) = optimize_leaf_functions(input);

        assert!(changed);
        assert!(!output.iter().any(|node| {
            matches!(
                node.instruction,
                Instruction::Push(Operand::StackPointer)
                    | Instruction::Push(Operand::ReturnAddress)
                    | Instruction::Pop(Operand::StackPointer)
                    | Instruction::Pop(Operand::ReturnAddress)
            )
        }));
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Jump(Operand::ReturnAddress))
            })
        );
        assert!(
            output
                .iter()
                .any(|node| { matches!(node.instruction, Instruction::Pop(Operand::Register(8))) })
        );
    }

    #[test]
    fn retains_frame_for_stack_using_leaf_function() {
        let input = simple_leaf(vec![Instruction::Sub(
            Operand::Register(0),
            Operand::StackPointer,
            Operand::Number(1.into()),
        )]);

        let (output, changed) = optimize_leaf_functions(input);

        assert!(!changed);
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::StackPointer))
            })
        );
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Pop(Operand::StackPointer))
            })
        );
    }

    #[test]
    fn retains_frame_for_function_with_nested_call() {
        let mut input = simple_leaf(Vec::new());
        let call = input
            .iter()
            .position(|node| matches!(node.instruction, Instruction::Push(Operand::ReturnAddress)))
            .unwrap();
        input.insert(
            call + 1,
            node(Instruction::JumpAndLink(Operand::Label(Cow::Borrowed(
                "other",
            )))),
        );

        let (output, changed) = optimize_leaf_functions(input);

        assert!(!changed);
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::StackPointer))
            })
        );
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Pop(Operand::ReturnAddress))
            })
        );
    }

    #[test]
    fn retains_frame_for_tuple_like_stack_return() {
        let input = simple_leaf(vec![Instruction::Push(Operand::Number(42.into()))]);

        let (output, changed) = optimize_leaf_functions(input);

        assert!(!changed);
        assert!(
            output.iter().any(|node| {
                matches!(node.instruction, Instruction::Push(Operand::StackPointer))
            })
        );
    }
}
