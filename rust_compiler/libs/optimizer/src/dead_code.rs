use il::{Instruction, InstructionNode, Operand};

/// Pass: Redundant Move Elimination  
/// Removes moves where source and destination are the same: `move rx rx`
pub fn remove_redundant_moves<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    for node in input {
        if let Instruction::Move(dst, src) = &node.instruction
            && dst == src
        {
            changed = true;
            continue;
        }
        output.push(node);
    }
    (output, changed)
}

/// Pass: Dead Code Elimination
/// Removes unreachable code after unconditional jumps.
pub fn remove_unreachable_code<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    let mut dead = false;
    for node in input {
        if let Instruction::LabelDef(_) = node.instruction {
            dead = false;
        }
        if dead {
            changed = true;
            continue;
        }
        if let Instruction::Jump(_) = node.instruction {
            dead = true
        }
        output.push(node);
    }
    (output, changed)
}

/// Pass: Remove Redundant Jumps
/// Removes jumps to the next instruction (after label resolution).
/// Must run AFTER label resolution since it needs line numbers.
/// This pass also adjusts all jump targets to account for removed instructions.
pub fn remove_redundant_jumps<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    let mut removed_lines = Vec::new();

    // First pass: identify redundant jumps
    for (i, node) in input.iter().enumerate() {
        // Check if this is a jump to the next line number
        if let Instruction::Jump(Operand::Number(target)) = &node.instruction {
            // Current line number is i, next line number is i+1
            // If jump target equals the next line, it's redundant
            if target.to_string().parse::<usize>().ok() == Some(i + 1) {
                changed = true;
                removed_lines.push(i);
                continue; // Skip this redundant jump
            }
        }
        output.push(node.clone());
    }

    // Second pass: adjust all jump/branch targets to account for removed lines
    if changed {
        for node in &mut output {
            // Helper to adjust a target line number
            let adjust_target = |target_line: usize| -> usize {
                // Count how many removed lines are before the target
                let offset = removed_lines
                    .iter()
                    .filter(|&&removed| removed < target_line)
                    .count();
                target_line.saturating_sub(offset)
            };

            match &mut node.instruction {
                Instruction::Jump(Operand::Number(target))
                | Instruction::JumpAndLink(Operand::Number(target)) => {
                    if let Ok(target_line) = target.to_string().parse::<usize>() {
                        *target = rust_decimal::Decimal::from(adjust_target(target_line));
                    }
                }
                Instruction::BranchEq(_, _, Operand::Number(target))
                | Instruction::BranchNe(_, _, Operand::Number(target))
                | Instruction::BranchGt(_, _, Operand::Number(target))
                | Instruction::BranchLt(_, _, Operand::Number(target))
                | Instruction::BranchGe(_, _, Operand::Number(target))
                | Instruction::BranchLe(_, _, Operand::Number(target))
                | Instruction::BranchEqZero(_, Operand::Number(target))
                | Instruction::BranchNeZero(_, Operand::Number(target)) => {
                    if let Ok(target_line) = target.to_string().parse::<usize>() {
                        *target = rust_decimal::Decimal::from(adjust_target(target_line));
                    }
                }
                _ => {}
            }
        }
    }

    (output, changed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use il::{Instruction, InstructionNode, Operand};

    #[test]
    fn test_remove_redundant_move() {
        let input = vec![InstructionNode::new(
            Instruction::Move(Operand::Register(1), Operand::Register(1)),
            None,
        )];

        let (output, changed) = remove_redundant_moves(input);
        assert!(changed);
        assert_eq!(output.len(), 0);
    }

    #[test]
    fn test_remove_unreachable() {
        let input = vec![
            InstructionNode::new(Instruction::Jump(Operand::Label("main".into())), None),
            InstructionNode::new(
                Instruction::Add(
                    Operand::Register(1),
                    Operand::Number(1.into()),
                    Operand::Number(2.into()),
                ),
                None,
            ),
            InstructionNode::new(Instruction::LabelDef("main".into()), None),
        ];

        let (output, changed) = remove_unreachable_code(input);
        assert!(changed);
        assert_eq!(output.len(), 2);
    }
}
