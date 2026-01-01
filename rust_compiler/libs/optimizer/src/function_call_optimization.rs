use crate::helpers::get_destination_reg;
use il::{Instruction, InstructionNode, Operand};
use rust_decimal::Decimal;
use std::collections::{HashMap, HashSet};

/// Analyzes which registers are written to by each function label.
fn analyze_clobbers(instructions: &[InstructionNode]) -> HashMap<String, HashSet<u8>> {
    let mut clobbers = HashMap::new();
    let mut current_label = None;

    for node in instructions {
        if let Instruction::LabelDef(label) = &node.instruction {
            current_label = Some(label.to_string());
            clobbers.insert(label.to_string(), HashSet::new());
        }

        if let Some(label) = &current_label
            && let Some(reg) = get_destination_reg(&node.instruction)
            && let Some(set) = clobbers.get_mut(label)
        {
            set.insert(reg);
        }
    }
    clobbers
}

/// Pass: Function Call Optimization
/// Removes Push/Restore pairs surrounding a JAL if the target function does not clobber that register.
pub fn optimize_function_calls<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let clobbers = analyze_clobbers(&input);
    let mut changed = false;
    let mut to_remove = HashSet::new();
    let mut stack_adjustments = HashMap::new();

    let mut i = 0;
    while i < input.len() {
        if let Instruction::JumpAndLink(Operand::Label(target)) = &input[i].instruction {
            let target_key = target.to_string();

            if let Some(func_clobbers) = clobbers.get(&target_key) {
                // 1. Identify Pushes immediately preceding the JAL
                let mut pushes = Vec::new(); // (index, register)
                let mut scan_back = i.saturating_sub(1);
                while scan_back > 0 {
                    if to_remove.contains(&scan_back) {
                        scan_back -= 1;
                        continue;
                    }
                    if let Instruction::Push(Operand::Register(r)) = &input[scan_back].instruction {
                        pushes.push((scan_back, *r));
                        scan_back -= 1;
                    } else {
                        break;
                    }
                }

                // 2. Identify Restores immediately following the JAL
                let mut restores = Vec::new(); // (index_of_get, register, index_of_sub)
                let mut scan_fwd = i + 1;
                while scan_fwd < input.len() {
                    // Skip 'sub r0 sp X'
                    if let Instruction::Sub(Operand::Register(0), Operand::StackPointer, _) =
                        &input[scan_fwd].instruction
                    {
                        // Check next instruction for the Get
                        if scan_fwd + 1 < input.len()
                            && let Instruction::Get(Operand::Register(r), _, Operand::Register(0)) =
                                &input[scan_fwd + 1].instruction
                        {
                            restores.push((scan_fwd + 1, *r, scan_fwd));
                            scan_fwd += 2;
                            continue;
                        }
                    }
                    break;
                }

                // 3. Stack Cleanup
                let cleanup_idx = scan_fwd;
                let has_cleanup = if cleanup_idx < input.len() {
                    matches!(
                        input[cleanup_idx].instruction,
                        Instruction::Sub(
                            Operand::StackPointer,
                            Operand::StackPointer,
                            Operand::Number(_)
                        )
                    )
                } else {
                    false
                };

                // SAFEGUARD: Check Counts!
                let mut push_counts = HashMap::new();
                for (_, r) in &pushes {
                    *push_counts.entry(*r).or_insert(0) += 1;
                }

                let mut restore_counts = HashMap::new();
                for (_, r, _) in &restores {
                    *restore_counts.entry(*r).or_insert(0) += 1;
                }

                let counts_match = push_counts
                    .iter()
                    .all(|(reg, count)| restore_counts.get(reg).unwrap_or(&0) == count);
                let counts_match_reverse = restore_counts
                    .iter()
                    .all(|(reg, count)| push_counts.get(reg).unwrap_or(&0) == count);

                // Clobber Check
                let all_pushes_safe = pushes.iter().all(|(_, r)| !func_clobbers.contains(r));

                if all_pushes_safe && has_cleanup && counts_match && counts_match_reverse {
                    // Remove all pushes/restores
                    for (p_idx, _) in pushes {
                        to_remove.insert(p_idx);
                    }
                    for (g_idx, _, s_idx) in restores {
                        to_remove.insert(g_idx);
                        to_remove.insert(s_idx);
                    }

                    // Reduce stack cleanup amount
                    let num_removed = push_counts.values().sum::<i32>() as i64;
                    stack_adjustments.insert(cleanup_idx, num_removed);
                    changed = true;
                }
            }
        }
        i += 1;
    }

    if changed {
        let mut clean = Vec::with_capacity(input.len());
        for (idx, mut node) in input.into_iter().enumerate() {
            if to_remove.contains(&idx) {
                continue;
            }

            // Apply stack adjustment
            if let Some(reduction) = stack_adjustments.get(&idx)
                && let Instruction::Sub(dst, a, Operand::Number(n)) = &node.instruction
            {
                let new_n = n - Decimal::from(*reduction);
                if new_n.is_zero() {
                    continue;
                }
                node.instruction = Instruction::Sub(dst.clone(), a.clone(), Operand::Number(new_n));
            }

            clean.push(node);
        }
        return (clean, changed);
    }

    (input, false)
}
