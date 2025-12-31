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
        // Never remove Pop instructions - they have critical side effects on the stack pointer
        if matches!(node.instruction, Instruction::Pop(_)) {
            continue;
        }

        if let Some(dest_reg) = get_destination_reg(&node.instruction) {
            // If this register was written before and hasn't been read, previous write is dead
            if let Some(&prev_idx) = last_write.get(&dest_reg) {
                // Check if the value was ever used between prev_idx and current
                let was_used = input[prev_idx + 1..i]
                    .iter()
                    .any(|n| reg_is_read_or_affects_control(&n.instruction, dest_reg))
                    // Also check if current instruction reads the register before overwriting it
                    || reg_is_read_or_affects_control(&node.instruction, dest_reg);

                if !was_used {
                    // Previous write was dead
                    to_remove.push(prev_idx);
                    changed = true;
                }
            }

            // Update last write location
            last_write.insert(dest_reg, i);
        }

        // Before clearing on labels/calls, check if current tracked writes are dead
        if matches!(
            node.instruction,
            Instruction::LabelDef(_) | Instruction::JumpAndLink(_)
        ) {
            // Check all currently tracked writes to see if they're dead
            for (&reg, &idx) in &last_write {
                // Don't remove writes to r15 (return register)
                if reg == 15 {
                    continue;
                }

                // Check if this write was used between write and now
                let was_used = input[idx + 1..i]
                    .iter()
                    .any(|n| reg_is_read_or_affects_control(&n.instruction, reg));

                if !was_used && !to_remove.contains(&idx) {
                    to_remove.push(idx);
                    changed = true;
                }
            }

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

/// Simplified check: Does this instruction read the register?
fn reg_is_read_or_affects_control(instr: &Instruction, reg: u8) -> bool {
    use crate::helpers::reg_is_read;

    // If it reads the register, it's used
    reg_is_read(instr, reg)
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
