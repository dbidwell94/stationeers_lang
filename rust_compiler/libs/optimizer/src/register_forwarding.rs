use crate::helpers::{get_destination_reg, reg_is_read, set_destination_reg};
use il::{Instruction, InstructionNode, Operand};
use std::collections::HashMap;

/// Pass: Register Forwarding
/// Eliminates intermediate moves by writing directly to the final destination.
/// Example: `l r1 d0 Temperature` + `move r9 r1` -> `l r9 d0 Temperature`
pub fn register_forwarding<'a>(
    mut input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut changed = false;
    let mut i = 0;

    // Build a map of label positions to detect backward jumps
    // Use String keys to avoid lifetime issues with references into input
    let label_positions: HashMap<String, usize> = input
        .iter()
        .enumerate()
        .filter_map(|(idx, node)| {
            if let Instruction::LabelDef(label) = &node.instruction {
                Some((label.to_string(), idx))
            } else {
                None
            }
        })
        .collect();

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
            // If we see a backward jump, we can still prove safety when the loop target
            // redefines temp_reg before any read on that path.
            let mut backward_jump_safe = true;
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

                // Function calls (jal) clobber the return register (r15)
                // So if we're tracking r15 and hit a function call, the old value is dead
                if matches!(node.instruction, Instruction::JumpAndLink(_)) && temp_reg == 15 {
                    break;
                }

                // Labels are just markers - they don't affect register liveness
                // But backward jumps create loops we need to analyze carefully
                let jump_target = match &node.instruction {
                    Instruction::Jump(Operand::Label(target)) => Some(target.as_ref()),
                    Instruction::BranchEq(_, _, Operand::Label(target))
                    | Instruction::BranchNe(_, _, Operand::Label(target))
                    | Instruction::BranchGt(_, _, Operand::Label(target))
                    | Instruction::BranchLt(_, _, Operand::Label(target))
                    | Instruction::BranchGe(_, _, Operand::Label(target))
                    | Instruction::BranchLe(_, _, Operand::Label(target))
                    | Instruction::BranchEqZero(_, Operand::Label(target))
                    | Instruction::BranchNeZero(_, Operand::Label(target)) => Some(target.as_ref()),
                    _ => None,
                };

                if let Some(target) = jump_target {
                    // Check if this is a backward jump (target appears before current position)
                    if let Some(&target_pos) = label_positions.get(target) {
                        if target_pos < i {
                            // Backward jump: conservatively safe only if temp_reg is
                            // redefined before any read along the linear path from target.
                            let mut loop_path_redefines_before_read = false;
                            for target_node in input.iter().skip(target_pos + 1) {
                                if reg_is_read(&target_node.instruction, temp_reg) {
                                    break;
                                }
                                if let Some(redef) = get_destination_reg(&target_node.instruction)
                                    && redef == temp_reg
                                {
                                    loop_path_redefines_before_read = true;
                                    break;
                                }
                            }

                            if !loop_path_redefines_before_read {
                                backward_jump_safe = false;
                                temp_is_dead = false;
                                break;
                            }
                        }
                        // Forward jump is OK - doesn't affect liveness before it
                    }
                }
            }

            if temp_is_dead && backward_jump_safe {
                // Safety check: ensure final_reg is not used as an operand in the current instruction.
                // This prevents generating invalid instructions like `sub r5 r0 r5` (read and write same register).
                if !reg_is_read(&input[i].instruction, final_reg) {
                    // Rewrite to use final destination directly
                    if let Some(new_instr) = set_destination_reg(&input[i].instruction, final_reg) {
                        input[i].instruction = new_instr;
                        input.remove(next_idx);
                        changed = true;
                        continue;
                    }
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

    #[test]
    fn test_forward_in_loop_when_redefined_before_read() {
        let input = vec![
            InstructionNode::new(Instruction::LabelDef("L".into()), None),
            InstructionNode::new(
                Instruction::Min(
                    Operand::Register(15),
                    Operand::Number(500.into()),
                    Operand::Number(400.into()),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(8), Operand::Register(15)),
                None,
            ),
            InstructionNode::new(Instruction::Jump(Operand::Label("L".into())), None),
        ];

        let (output, changed) = register_forwarding(input);
        assert!(changed);
        assert_eq!(output.len(), 3);
        assert!(matches!(
            output[1].instruction,
            Instruction::Min(Operand::Register(8), _, _)
        ));
    }

    #[test]
    fn test_do_not_forward_in_loop_when_read_before_redefine() {
        let input = vec![
            InstructionNode::new(Instruction::LabelDef("L".into()), None),
            InstructionNode::new(
                Instruction::Add(
                    Operand::Register(1),
                    Operand::Register(15),
                    Operand::Number(1.into()),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::Min(
                    Operand::Register(15),
                    Operand::Number(500.into()),
                    Operand::Number(400.into()),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(8), Operand::Register(15)),
                None,
            ),
            InstructionNode::new(Instruction::Jump(Operand::Label("L".into())), None),
        ];

        let (output, changed) = register_forwarding(input);
        assert!(!changed);
        assert_eq!(output.len(), 5);
        assert!(matches!(
            output[3].instruction,
            Instruction::Move(Operand::Register(8), Operand::Register(15))
        ));
    }

    #[test]
    fn test_forward_before_jal_clobber_of_r15() {
        let input = vec![
            InstructionNode::new(
                Instruction::Min(
                    Operand::Register(15),
                    Operand::Number(500.into()),
                    Operand::Number(400.into()),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(8), Operand::Register(15)),
                None,
            ),
            InstructionNode::new(Instruction::JumpAndLink(Operand::Label("f".into())), None),
        ];

        let (output, changed) = register_forwarding(input);
        assert!(changed);
        assert_eq!(output.len(), 2);
        assert!(matches!(
            output[0].instruction,
            Instruction::Min(Operand::Register(8), _, _)
        ));
        assert!(matches!(
            output[1].instruction,
            Instruction::JumpAndLink(Operand::Label(_))
        ));
    }

    #[test]
    fn test_forward_select_move_in_loop_when_redefined_before_read() {
        let input = vec![
            InstructionNode::new(Instruction::LabelDef("L".into()), None),
            InstructionNode::new(
                Instruction::Select(
                    Operand::Register(6),
                    Operand::Register(4),
                    Operand::Number(2.into()),
                    Operand::Register(5),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(9), Operand::Register(6)),
                None,
            ),
            InstructionNode::new(Instruction::Jump(Operand::Label("L".into())), None),
        ];

        let (output, changed) = register_forwarding(input);
        assert!(changed);
        assert_eq!(output.len(), 3);
        assert!(matches!(
            output[1].instruction,
            Instruction::Select(Operand::Register(9), _, _, _)
        ));
    }

    #[test]
    fn test_forwarding_does_not_modify_push_pop_balance() {
        let input = vec![
            InstructionNode::new(Instruction::LabelDef("L".into()), None),
            InstructionNode::new(Instruction::Push(Operand::Register(8)), None),
            InstructionNode::new(
                Instruction::Min(
                    Operand::Register(15),
                    Operand::Number(500.into()),
                    Operand::Number(400.into()),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(8), Operand::Register(15)),
                None,
            ),
            InstructionNode::new(Instruction::Pop(Operand::Register(8)), None),
            InstructionNode::new(Instruction::Jump(Operand::Label("L".into())), None),
        ];

        let push_before = input
            .iter()
            .filter(|n| matches!(n.instruction, Instruction::Push(_)))
            .count();
        let pop_before = input
            .iter()
            .filter(|n| matches!(n.instruction, Instruction::Pop(_)))
            .count();

        let (output, changed) = register_forwarding(input);
        assert!(changed);

        let push_after = output
            .iter()
            .filter(|n| matches!(n.instruction, Instruction::Push(_)))
            .count();
        let pop_after = output
            .iter()
            .filter(|n| matches!(n.instruction, Instruction::Pop(_)))
            .count();

        assert_eq!(push_before, push_after);
        assert_eq!(pop_before, pop_after);
    }
}
