use crate::helpers::{get_destination_reg, reg_is_read, set_destination_reg};
use il::{Instruction, InstructionNode};

/// Pass: Register Forwarding
/// Eliminates intermediate moves by writing directly to the final destination.
/// Example: `l r1 d0 Temperature` + `move r9 r1` -> `l r9 d0 Temperature`
pub fn register_forwarding<'a>(
    mut input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut changed = false;
    let mut i = 0;

    while i < input.len().saturating_sub(1) {
        let next_idx = i + 1;

        // Check if current instruction defines a register
        // and the NEXT instruction is a move from that register.
        let forward_candidate = if let Some(def_reg) = get_destination_reg(&input[i].instruction) {
            if let Instruction::Move(
                il::Operand::Register(dest_reg),
                il::Operand::Register(src_reg),
            ) = &input[next_idx].instruction
            {
                if *src_reg == def_reg {
                    Some((def_reg, *dest_reg))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        if let Some((temp_reg, final_reg)) = forward_candidate {
            // Check liveness: Is temp_reg used after i+1?
            let mut temp_is_dead = true;
            for node in input.iter().skip(i + 2) {
                if reg_is_read(&node.instruction, temp_reg) {
                    temp_is_dead = false;
                    break;
                }
                // If the temp is redefined, then the old value is dead
                if let Some(redef) = get_destination_reg(&node.instruction)
                    && redef == temp_reg
                {
                    break;
                }

                // Conservative: assume liveness might leak at labels/jumps
                if matches!(
                    node.instruction,
                    Instruction::LabelDef(_) | Instruction::Jump(_) | Instruction::JumpAndLink(_)
                ) {
                    temp_is_dead = false;
                    break;
                }
            }

            if temp_is_dead {
                // Rewrite to use final destination directly
                if let Some(new_instr) = set_destination_reg(&input[i].instruction, final_reg) {
                    input[i].instruction = new_instr;
                    input.remove(next_idx);
                    changed = true;
                    continue;
                }
            }
        }

        i += 1;
    }

    (input, changed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use il::{Instruction, InstructionNode, Operand};

    #[test]
    fn test_forward_simple_move() {
        let input = vec![
            InstructionNode::new(
                Instruction::Add(
                    Operand::Register(1),
                    Operand::Register(2),
                    Operand::Register(3),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(5), Operand::Register(1)),
                None,
            ),
        ];

        let (output, changed) = register_forwarding(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        assert!(matches!(
            output[0].instruction,
            Instruction::Add(Operand::Register(5), _, _)
        ));
    }
}
