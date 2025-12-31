use il::{Instruction, InstructionNode, Operand};

/// Pass: Peephole Optimization
/// Recognizes and optimizes common instruction patterns.
pub fn peephole_optimization<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    let mut i = 0;

    while i < input.len() {
        // Pattern: push sp; push ra ... pop ra; pop sp (with no jal in between)
        // If we push sp and ra and later pop them, but never call a function in between, remove all four
        // and adjust any stack pointer offsets in between by -2
        if i + 1 < input.len() {
            if let (
                Instruction::Push(Operand::StackPointer),
                Instruction::Push(Operand::ReturnAddress),
            ) = (&input[i].instruction, &input[i + 1].instruction)
            {
                // Look for matching pop ra; pop sp pattern
                if let Some((ra_pop_idx, instructions_between)) =
                    find_matching_ra_pop(&input[i + 1..])
                {
                    let absolute_ra_pop = i + 1 + ra_pop_idx;
                    // Check if the next instruction is pop sp
                    if absolute_ra_pop + 1 < input.len() {
                        if let Instruction::Pop(Operand::StackPointer) =
                            &input[absolute_ra_pop + 1].instruction
                        {
                            // Check if there's any jal between push and pop
                            let has_call = instructions_between.iter().any(|node| {
                                matches!(node.instruction, Instruction::JumpAndLink(_))
                            });

                            if !has_call {
                                // Safe to remove all four: push sp, push ra, pop ra, pop sp
                                // Also need to adjust stack pointer offsets in between by -2
                                let absolute_sp_pop = absolute_ra_pop + 1;
                                // Clear output since we're going to reprocess the entire input
                                output.clear();
                                for (idx, node) in input.iter().enumerate() {
                                    if idx == i
                                        || idx == i + 1
                                        || idx == absolute_ra_pop
                                        || idx == absolute_sp_pop
                                    {
                                        // Skip all four push/pop instructions
                                        continue;
                                    }

                                    // If this instruction is between the pushes and pops, adjust its stack offsets
                                    if idx > i + 1 && idx < absolute_ra_pop {
                                        let adjusted_instruction =
                                            adjust_stack_offset(node.instruction.clone(), 2);
                                        output.push(InstructionNode::new(
                                            adjusted_instruction,
                                            node.span,
                                        ));
                                    } else {
                                        output.push(node.clone());
                                    }
                                }
                                changed = true;
                                // We've processed the entire input, so break
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Pattern: push ra ... pop ra (with no jal in between)
        // Fallback for when there's only ra push/pop without sp
        if let Instruction::Push(Operand::ReturnAddress) = &input[i].instruction {
            if let Some((pop_idx, instructions_between)) = find_matching_ra_pop(&input[i..]) {
                // Check if there's any jal between push and pop
                let has_call = instructions_between
                    .iter()
                    .any(|node| matches!(node.instruction, Instruction::JumpAndLink(_)));

                if !has_call {
                    // Safe to remove both push and pop
                    // Also need to adjust stack pointer offsets in between
                    let absolute_pop_idx = i + pop_idx;
                    // Clear output since we're going to reprocess the entire input
                    output.clear();
                    for (idx, node) in input.iter().enumerate() {
                        if idx == i || idx == absolute_pop_idx {
                            // Skip the push and pop
                            continue;
                        }

                        // If this instruction is between push and pop, adjust its stack offsets
                        if idx > i && idx < absolute_pop_idx {
                            let adjusted_instruction =
                                adjust_stack_offset(node.instruction.clone(), 1);
                            output.push(InstructionNode::new(adjusted_instruction, node.span));
                        } else {
                            output.push(node.clone());
                        }
                    }
                    changed = true;
                    // We've processed the entire input, so break
                    break;
                }
            }
        }

        // Pattern: Branch-Move-Jump-Label-Move-Label -> Select
        // beqz r1 else_label
        // move r2 val1
        // j end_label
        // else_label:
        // move r2 val2
        // end_label:
        // Converts to: select r2 r1 val1 val2
        if i + 5 < input.len() {
            let select_pattern = try_match_select_pattern(&input[i..i + 6]);
            if let Some((dst, cond, true_val, false_val, skip_count)) = select_pattern {
                output.push(InstructionNode::new(
                    Instruction::Select(dst, cond, true_val, false_val),
                    input[i].span,
                ));
                changed = true;
                i += skip_count;
                continue;
            }
        }

        // Pattern: seq + beqz -> beq
        if i + 1 < input.len() {
            let pattern = match (&input[i].instruction, &input[i + 1].instruction) {
                (
                    Instruction::SetEq(Operand::Register(temp), a, b),
                    Instruction::BranchEqZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Eq, true)), // invert: beqz means "if NOT equal"

                (
                    Instruction::SetNe(Operand::Register(temp), a, b),
                    Instruction::BranchEqZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Ne, true)),

                (
                    Instruction::SetGt(Operand::Register(temp), a, b),
                    Instruction::BranchEqZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Gt, true)),

                (
                    Instruction::SetLt(Operand::Register(temp), a, b),
                    Instruction::BranchEqZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Lt, true)),

                (
                    Instruction::SetGe(Operand::Register(temp), a, b),
                    Instruction::BranchEqZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Ge, true)),

                (
                    Instruction::SetLe(Operand::Register(temp), a, b),
                    Instruction::BranchEqZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Le, true)),

                // Pattern: seq + bnez -> bne
                (
                    Instruction::SetEq(Operand::Register(temp), a, b),
                    Instruction::BranchNeZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Eq, false)),

                (
                    Instruction::SetNe(Operand::Register(temp), a, b),
                    Instruction::BranchNeZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Ne, false)),

                (
                    Instruction::SetGt(Operand::Register(temp), a, b),
                    Instruction::BranchNeZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Gt, false)),

                (
                    Instruction::SetLt(Operand::Register(temp), a, b),
                    Instruction::BranchNeZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Lt, false)),

                (
                    Instruction::SetGe(Operand::Register(temp), a, b),
                    Instruction::BranchNeZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Ge, false)),

                (
                    Instruction::SetLe(Operand::Register(temp), a, b),
                    Instruction::BranchNeZero(Operand::Register(cond), label),
                ) if temp == cond => Some((a, b, label, BranchType::Le, false)),

                _ => None,
            };

            if let Some((a, b, label, branch_type, invert)) = pattern {
                // Create optimized branch instruction
                let new_instr = if invert {
                    // beqz after seq means "branch if NOT equal" -> bne
                    match branch_type {
                        BranchType::Eq => {
                            Instruction::BranchNe(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Ne => {
                            Instruction::BranchEq(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Gt => {
                            Instruction::BranchLe(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Lt => {
                            Instruction::BranchGe(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Ge => {
                            Instruction::BranchLt(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Le => {
                            Instruction::BranchGt(a.clone(), b.clone(), label.clone())
                        }
                    }
                } else {
                    // bnez after seq means "branch if equal" -> beq
                    match branch_type {
                        BranchType::Eq => {
                            Instruction::BranchEq(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Ne => {
                            Instruction::BranchNe(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Gt => {
                            Instruction::BranchGt(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Lt => {
                            Instruction::BranchLt(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Ge => {
                            Instruction::BranchGe(a.clone(), b.clone(), label.clone())
                        }
                        BranchType::Le => {
                            Instruction::BranchLe(a.clone(), b.clone(), label.clone())
                        }
                    }
                };

                output.push(InstructionNode::new(new_instr, input[i].span));
                changed = true;
                i += 2; // Skip both instructions
                continue;
            }
        }

        output.push(input[i].clone());
        i += 1;
    }

    (output, changed)
}

/// Tries to match a select pattern in the instruction sequence.
/// Pattern (6 instructions):
///   beqz/bnez cond else_label   (i+0)
///   move dst val1               (i+1)
///   j end_label                 (i+2)
///   else_label:                 (i+3)
///   move dst val2               (i+4)
///   end_label:                  (i+5)
/// Returns: (dst, cond, true_val, false_val, instruction_count)
fn try_match_select_pattern<'a>(
    instructions: &[InstructionNode<'a>],
) -> Option<(Operand<'a>, Operand<'a>, Operand<'a>, Operand<'a>, usize)> {
    if instructions.len() < 6 {
        return None;
    }

    // Check for beqz pattern
    if let Instruction::BranchEqZero(cond, Operand::Label(else_label)) =
        &instructions[0].instruction
    {
        if let Instruction::Move(dst1, val1) = &instructions[1].instruction {
            if let Instruction::Jump(Operand::Label(end_label)) = &instructions[2].instruction {
                if let Instruction::LabelDef(label3) = &instructions[3].instruction {
                    if label3 == else_label {
                        if let Instruction::Move(dst2, val2) = &instructions[4].instruction {
                            if dst1 == dst2 {
                                if let Instruction::LabelDef(label5) = &instructions[5].instruction
                                {
                                    if label5 == end_label {
                                        // beqz means: if cond==0, goto else, so val1 is for true, val2 for false
                                        // select dst cond true_val false_val
                                        // When cond is non-zero (true), use val1, otherwise val2
                                        return Some((
                                            dst1.clone(),
                                            cond.clone(),
                                            val1.clone(),
                                            val2.clone(),
                                            6,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Check for bnez pattern
    if let Instruction::BranchNeZero(cond, Operand::Label(then_label)) =
        &instructions[0].instruction
    {
        if let Instruction::Move(dst1, val_false) = &instructions[1].instruction {
            if let Instruction::Jump(Operand::Label(end_label)) = &instructions[2].instruction {
                if let Instruction::LabelDef(label3) = &instructions[3].instruction {
                    if label3 == then_label {
                        if let Instruction::Move(dst2, val_true) = &instructions[4].instruction {
                            if dst1 == dst2 {
                                if let Instruction::LabelDef(label5) = &instructions[5].instruction
                                {
                                    if label5 == end_label {
                                        // bnez means: if cond!=0, goto then, so val_true for true, val_false for false
                                        return Some((
                                            dst1.clone(),
                                            cond.clone(),
                                            val_true.clone(),
                                            val_false.clone(),
                                            6,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    None
}

/// Finds a matching `pop ra` for a `push ra` at the start of the slice.
/// Returns the index of the pop and the instructions in between.
fn find_matching_ra_pop<'a>(
    instructions: &'a [InstructionNode<'a>],
) -> Option<(usize, &'a [InstructionNode<'a>])> {
    if instructions.is_empty() {
        return None;
    }

    // Skip the push itself
    for (idx, node) in instructions.iter().enumerate().skip(1) {
        if let Instruction::Pop(Operand::ReturnAddress) = &node.instruction {
            // Found matching pop
            return Some((idx, &instructions[1..idx]));
        }

        // Stop searching if we hit a jump (different control flow)
        // Labels are OK - they're just markers
        if matches!(
            node.instruction,
            Instruction::Jump(_) | Instruction::JumpRelative(_)
        ) {
            return None;
        }
    }

    None
}

/// Checks if an instruction uses or modifies the stack pointer.
#[allow(dead_code)]
fn uses_stack_pointer(instruction: &Instruction) -> bool {
    match instruction {
        Instruction::Push(_) | Instruction::Pop(_) | Instruction::Peek(_) => true,
        Instruction::Add(Operand::StackPointer, _, _)
        | Instruction::Sub(Operand::StackPointer, _, _)
        | Instruction::Mul(Operand::StackPointer, _, _)
        | Instruction::Div(Operand::StackPointer, _, _)
        | Instruction::Mod(Operand::StackPointer, _, _) => true,
        Instruction::Add(_, Operand::StackPointer, _)
        | Instruction::Sub(_, Operand::StackPointer, _)
        | Instruction::Mul(_, Operand::StackPointer, _)
        | Instruction::Div(_, Operand::StackPointer, _)
        | Instruction::Mod(_, Operand::StackPointer, _) => true,
        Instruction::Add(_, _, Operand::StackPointer)
        | Instruction::Sub(_, _, Operand::StackPointer)
        | Instruction::Mul(_, _, Operand::StackPointer)
        | Instruction::Div(_, _, Operand::StackPointer)
        | Instruction::Mod(_, _, Operand::StackPointer) => true,
        Instruction::Move(Operand::StackPointer, _)
        | Instruction::Move(_, Operand::StackPointer) => true,
        _ => false,
    }
}

/// Adjusts stack pointer offsets in an instruction by decrementing them by a given amount.
/// This is necessary when removing push operations that would have increased the stack size.
fn adjust_stack_offset<'a>(instruction: Instruction<'a>, decrement: i64) -> Instruction<'a> {
    use rust_decimal::prelude::*;

    match instruction {
        // Adjust arithmetic operations on sp that use literal offsets
        Instruction::Sub(dst, Operand::StackPointer, Operand::Number(n)) => {
            let new_n = n - Decimal::from(decrement);
            // If the result is 0 or negative, we may want to skip this entirely
            // but for now, just adjust the value
            Instruction::Sub(dst, Operand::StackPointer, Operand::Number(new_n))
        }
        Instruction::Add(dst, Operand::StackPointer, Operand::Number(n)) => {
            let new_n = n - Decimal::from(decrement);
            Instruction::Add(dst, Operand::StackPointer, Operand::Number(new_n))
        }
        // Return the instruction unchanged if it doesn't need adjustment
        other => other,
    }
}

#[derive(Debug, Clone, Copy)]
enum BranchType {
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_seq_beqz_to_bne() {
        let input = vec![
            InstructionNode::new(
                Instruction::SetEq(
                    Operand::Register(1),
                    Operand::Register(2),
                    Operand::Register(3),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::BranchEqZero(Operand::Register(1), Operand::Label("target".into())),
                None,
            ),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        assert!(matches!(
            output[0].instruction,
            Instruction::BranchNe(_, _, _)
        ));
    }

    #[test]
    fn test_sne_beqz_to_beq() {
        let input = vec![
            InstructionNode::new(
                Instruction::SetNe(
                    Operand::Register(1),
                    Operand::Register(2),
                    Operand::Register(3),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::BranchEqZero(Operand::Register(1), Operand::Label("target".into())),
                None,
            ),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        assert!(matches!(
            output[0].instruction,
            Instruction::BranchEq(_, _, _)
        ));
    }

    #[test]
    fn test_seq_bnez_to_beq() {
        let input = vec![
            InstructionNode::new(
                Instruction::SetEq(
                    Operand::Register(1),
                    Operand::Register(2),
                    Operand::Register(3),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::BranchNeZero(Operand::Register(1), Operand::Label("target".into())),
                None,
            ),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        assert!(matches!(
            output[0].instruction,
            Instruction::BranchEq(_, _, _)
        ));
    }

    #[test]
    fn test_sgt_beqz_to_ble() {
        let input = vec![
            InstructionNode::new(
                Instruction::SetGt(
                    Operand::Register(1),
                    Operand::Register(2),
                    Operand::Register(3),
                ),
                None,
            ),
            InstructionNode::new(
                Instruction::BranchEqZero(Operand::Register(1), Operand::Label("target".into())),
                None,
            ),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        assert!(matches!(
            output[0].instruction,
            Instruction::BranchLe(_, _, _)
        ));
    }

    #[test]
    fn test_branch_move_jump_to_select_beqz() {
        // Pattern: beqz r1 else / move r2 10 / j end / else: / move r2 20 / end:
        // Should convert to: select r2 r1 10 20
        let input = vec![
            InstructionNode::new(
                Instruction::BranchEqZero(Operand::Register(1), Operand::Label("else".into())),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(2), Operand::Number(10.into())),
                None,
            ),
            InstructionNode::new(Instruction::Jump(Operand::Label("end".into())), None),
            InstructionNode::new(Instruction::LabelDef("else".into()), None),
            InstructionNode::new(
                Instruction::Move(Operand::Register(2), Operand::Number(20.into())),
                None,
            ),
            InstructionNode::new(Instruction::LabelDef("end".into()), None),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        if let Instruction::Select(dst, cond, true_val, false_val) = &output[0].instruction {
            assert!(matches!(dst, Operand::Register(2)));
            assert!(matches!(cond, Operand::Register(1)));
            assert!(matches!(true_val, Operand::Number(_)));
            assert!(matches!(false_val, Operand::Number(_)));
        } else {
            panic!("Expected Select instruction");
        }
    }

    #[test]
    fn test_branch_move_jump_to_select_bnez() {
        // Pattern: bnez r1 then / move r2 20 / j end / then: / move r2 10 / end:
        // Should convert to: select r2 r1 10 20
        let input = vec![
            InstructionNode::new(
                Instruction::BranchNeZero(Operand::Register(1), Operand::Label("then".into())),
                None,
            ),
            InstructionNode::new(
                Instruction::Move(Operand::Register(2), Operand::Number(20.into())),
                None,
            ),
            InstructionNode::new(Instruction::Jump(Operand::Label("end".into())), None),
            InstructionNode::new(Instruction::LabelDef("then".into()), None),
            InstructionNode::new(
                Instruction::Move(Operand::Register(2), Operand::Number(10.into())),
                None,
            ),
            InstructionNode::new(Instruction::LabelDef("end".into()), None),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        if let Instruction::Select(dst, cond, true_val, false_val) = &output[0].instruction {
            assert!(matches!(dst, Operand::Register(2)));
            assert!(matches!(cond, Operand::Register(1)));
            assert!(matches!(true_val, Operand::Number(_)));
            assert!(matches!(false_val, Operand::Number(_)));
        } else {
            panic!("Expected Select instruction");
        }
    }

    #[test]
    fn test_remove_useless_ra_push_pop() {
        // Pattern: push ra / add r1 r2 r3 / pop ra
        // Should remove both push and pop since no jal in between
        let input = vec![
            InstructionNode::new(Instruction::Push(Operand::ReturnAddress), None),
            InstructionNode::new(
                Instruction::Add(
                    Operand::Register(1),
                    Operand::Register(2),
                    Operand::Register(3),
                ),
                None,
            ),
            InstructionNode::new(Instruction::Pop(Operand::ReturnAddress), None),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        assert!(matches!(output[0].instruction, Instruction::Add(_, _, _)));
    }

    #[test]
    fn test_keep_ra_push_pop_with_jal() {
        // Pattern: push ra / jal func / pop ra
        // Should keep both since there's a jal in between
        let input = vec![
            InstructionNode::new(Instruction::Push(Operand::ReturnAddress), None),
            InstructionNode::new(
                Instruction::JumpAndLink(Operand::Label("func".into())),
                None,
            ),
            InstructionNode::new(Instruction::Pop(Operand::ReturnAddress), None),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(!changed);
        assert_eq!(output.len(), 3);
    }

    #[test]
    fn test_ra_push_pop_with_stack_offset_adjustment() {
        // Pattern: push ra / sub r1 sp 2 / pop ra
        // Should remove push/pop AND adjust the stack offset from 2 to 1
        use rust_decimal::prelude::*;

        let input = vec![
            InstructionNode::new(Instruction::Push(Operand::ReturnAddress), None),
            InstructionNode::new(
                Instruction::Sub(
                    Operand::Register(1),
                    Operand::StackPointer,
                    Operand::Number(Decimal::from(2)),
                ),
                None,
            ),
            InstructionNode::new(Instruction::Pop(Operand::ReturnAddress), None),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);

        if let Instruction::Sub(dst, src, Operand::Number(offset)) = &output[0].instruction {
            assert!(matches!(dst, Operand::Register(1)));
            assert!(matches!(src, Operand::StackPointer));
            assert_eq!(*offset, Decimal::from(1)); // Should be decremented from 2 to 1
        } else {
            panic!("Expected Sub instruction with adjusted offset");
        }
    }

    #[test]
    fn test_remove_sp_and_ra_push_pop() {
        // Pattern: push sp / push ra / move r8 10 / pop ra / pop sp
        // Should remove all four push/pop instructions since no jal in between
        let input = vec![
            InstructionNode::new(Instruction::Push(Operand::StackPointer), None),
            InstructionNode::new(Instruction::Push(Operand::ReturnAddress), None),
            InstructionNode::new(
                Instruction::Move(Operand::Register(8), Operand::Number(10.into())),
                None,
            ),
            InstructionNode::new(Instruction::Pop(Operand::ReturnAddress), None),
            InstructionNode::new(Instruction::Pop(Operand::StackPointer), None),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        assert!(matches!(
            output[0].instruction,
            Instruction::Move(Operand::Register(8), _)
        ));
    }

    #[test]
    fn test_keep_sp_and_ra_push_pop_with_jal() {
        // Pattern: push sp / push ra / jal func / pop ra / pop sp
        // Should keep all since there's a jal in between
        let input = vec![
            InstructionNode::new(Instruction::Push(Operand::StackPointer), None),
            InstructionNode::new(Instruction::Push(Operand::ReturnAddress), None),
            InstructionNode::new(
                Instruction::JumpAndLink(Operand::Label("func".into())),
                None,
            ),
            InstructionNode::new(Instruction::Pop(Operand::ReturnAddress), None),
            InstructionNode::new(Instruction::Pop(Operand::StackPointer), None),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(!changed);
        assert_eq!(output.len(), 5);
    }

    #[test]
    fn test_sp_and_ra_with_stack_offset_adjustment() {
        // Pattern: push sp / push ra / sub r1 sp 3 / pop ra / pop sp
        // Should remove all push/pop AND adjust the stack offset from 3 to 1 (decrement by 2)
        use rust_decimal::prelude::*;

        let input = vec![
            InstructionNode::new(Instruction::Push(Operand::StackPointer), None),
            InstructionNode::new(Instruction::Push(Operand::ReturnAddress), None),
            InstructionNode::new(
                Instruction::Sub(
                    Operand::Register(1),
                    Operand::StackPointer,
                    Operand::Number(Decimal::from(3)),
                ),
                None,
            ),
            InstructionNode::new(Instruction::Pop(Operand::ReturnAddress), None),
            InstructionNode::new(Instruction::Pop(Operand::StackPointer), None),
        ];

        let (output, changed) = peephole_optimization(input);
        assert!(changed);
        assert_eq!(output.len(), 1);

        if let Instruction::Sub(dst, src, Operand::Number(offset)) = &output[0].instruction {
            assert!(matches!(dst, Operand::Register(1)));
            assert!(matches!(src, Operand::StackPointer));
            assert_eq!(*offset, Decimal::from(1)); // Should be decremented from 3 to 1
        } else {
            panic!("Expected Sub instruction with adjusted offset");
        }
    }
}
