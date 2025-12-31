use crate::helpers::get_destination_reg;
use il::{Instruction, InstructionNode};
use std::collections::HashMap;

/// Pass: Dead Store Elimination
/// Removes writes to registers that are never read before being overwritten.
pub fn dead_store_elimination<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut changed = false;
    let mut last_write: HashMap<u8, usize> = HashMap::new();
    let mut to_remove = Vec::new();

    // Scan for dead writes
    for (i, node) in input.iter().enumerate() {
        if let Some(dest_reg) = get_destination_reg(&node.instruction) {
            // If this register was written before and hasn't been read, previous write is dead
            if let Some(&prev_idx) = last_write.get(&dest_reg) {
                // Check if the value was ever used between prev_idx and current
                let was_used = input[prev_idx + 1..i]
                    .iter()
                    .any(|n| reg_is_read_or_affects_control(&n.instruction, dest_reg));

                if !was_used {
                    // Previous write was dead
                    to_remove.push(prev_idx);
                    changed = true;
                }
            }

            // Update last write location
            last_write.insert(dest_reg, i);
        }

        // On labels/jumps, conservatively clear tracking (value might be used elsewhere)
        if matches!(
            node.instruction,
            Instruction::LabelDef(_) | Instruction::Jump(_) | Instruction::JumpAndLink(_)
        ) {
            last_write.clear();
        }
    }

    if changed {
        let output = input
            .into_iter()
            .enumerate()
            .filter_map(|(i, node)| {
                if to_remove.contains(&i) {
                    None
                } else {
                    Some(node)
                }
            })
            .collect();
        (output, true)
    } else {
        (input, false)
    }
}

/// Simplified check: Does this instruction read the register or affect control flow?
fn reg_is_read_or_affects_control(instr: &Instruction, reg: u8) -> bool {
    use crate::helpers::reg_is_read;

    // If it reads the register, it's used
    if reg_is_read(instr, reg) {
        return true;
    }

    // Conservatively assume register might be used if there's control flow
    matches!(
        instr,
        Instruction::Jump(_)
            | Instruction::JumpAndLink(_)
            | Instruction::BranchEq(_, _, _)
            | Instruction::BranchNe(_, _, _)
            | Instruction::BranchGt(_, _, _)
            | Instruction::BranchLt(_, _, _)
            | Instruction::BranchGe(_, _, _)
            | Instruction::BranchLe(_, _, _)
            | Instruction::BranchEqZero(_, _)
            | Instruction::BranchNeZero(_, _)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use il::Operand;

    #[test]
    fn test_dead_store() {
        let input = vec![
            InstructionNode::new(
                Instruction::Move(Operand::Register(1), Operand::Number(5.into())),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(1), Operand::Number(10.into())),
                None,
            ),
        ];

        let (output, changed) = dead_store_elimination(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
    }
}
