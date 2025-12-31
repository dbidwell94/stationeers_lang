use crate::helpers::get_destination_reg;
use il::{Instruction, InstructionNode, Operand};
use std::collections::HashMap;

/// Pass: Dead Store Elimination
/// Removes writes to registers that are never read before being overwritten.
pub fn dead_store_elimination<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    // Forward pass: Remove writes that are immediately overwritten
    let (input, forward_changed) = eliminate_overwritten_stores(input);

    // Note: Backward pass disabled for now - it needs more work to handle all cases correctly
    // The forward pass is sufficient for most common patterns
    // (e.g., move r6 r15 immediately followed by move r6 r15 again)

    (input, forward_changed)
}

/// Forward pass: Remove stores that are overwritten before being read
fn eliminate_overwritten_stores<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
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
                }
            }

            // Update last write location
            last_write.insert(dest_reg, i);
        }

        // Handle control flow instructions
        match &node.instruction {
            // JumpAndLink (function calls) only clobbers the return register (r15)
            // We can continue tracking other registers across function calls
            Instruction::JumpAndLink(_) => {
                last_write.remove(&15);
            }
            // Other control flow instructions create complexity - clear all tracking
            Instruction::Jump(_)
            | Instruction::LabelDef(_)
            | Instruction::BranchEq(_, _, _)
            | Instruction::BranchNe(_, _, _)
            | Instruction::BranchGt(_, _, _)
            | Instruction::BranchLt(_, _, _)
            | Instruction::BranchGe(_, _, _)
            | Instruction::BranchLe(_, _, _)
            | Instruction::BranchEqZero(_, _)
            | Instruction::BranchNeZero(_, _) => {
                last_write.clear();
            }
            _ => {}
        }
    }

    if !to_remove.is_empty() {
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

/// Backward pass: Remove stores that are never read before function return
fn eliminate_unread_stores<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    use std::collections::HashSet;
    let mut changed = false;
    let mut to_remove = Vec::new();
    
    // Find function boundaries by matching push/pop pairs of sp (register 17)
    let mut function_ranges = Vec::new();
    let mut stack = Vec::new();
    
    for (i, node) in input.iter().enumerate() {
        match &node.instruction {
            Instruction::Push(Operand::Register(17)) => {
                stack.push(i);
            }
            Instruction::Pop(Operand::Register(17)) => {
                if let Some(start) = stack.pop() {
                    // Find the j ra after the pop sp
                    let mut end = i;
                    for j in (i + 1)..input.len() {
                        if matches!(input[j].instruction, Instruction::Jump(Operand::Register(16))) {
                            end = j;
                            break;
                        }
                    }
                    function_ranges.push((start, end));
                }
            }
            _ => {}
        }
    }

    // Process each function independently
    for (func_start, func_end) in function_ranges {
        // Process this function backward
        let mut live_registers: HashSet<u8> = HashSet::new();
        
        // First pass: find which registers are actually read in the function
        for i in func_start..=func_end {
            let node = &input[i];
            for reg in 0..16 {
                if crate::helpers::reg_is_read(&node.instruction, reg) {
                    live_registers.insert(reg);
                }
            }
        }
        
        // Clear live registers - we'll rebuild it as we go backward
        live_registers.clear();
        
        // Start from the end of the function, working backward
        for i in (func_start..=func_end).rev() {
            let node = &input[i];
            
            // Skip stack management instructions
            if matches!(node.instruction, Instruction::Push(_) | Instruction::Pop(_)) {
                continue;
            }

            // If instruction writes to a register (assignment/computation)
            if let Some(dest_reg) = get_destination_reg(&node.instruction) {
                // If the register isn't live (not read after this write), this write is dead
                if !live_registers.contains(&dest_reg) {
                    to_remove.push(i);
                    changed = true;
                    // Don't process the reads of this dead instruction
                    continue;
                } else {
                    // This instruction is live. Check what it reads and marks as live.
                    for reg in 0..16 {
                        if crate::helpers::reg_is_read(&node.instruction, reg) {
                            live_registers.insert(reg);
                        }
                    }
                    // This instruction defines the register, so remove it from live set
                    live_registers.remove(&dest_reg);
                }
            } else {
                // Instruction doesn't write - just track reads
                for reg in 0..16 {
                    if crate::helpers::reg_is_read(&node.instruction, reg) {
                        live_registers.insert(reg);
                    }
                }
            }
        }
    }

    if !to_remove.is_empty() {
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
        (input, changed)
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

    #[test]
    fn test_dead_store_in_function() {
        // Test that dead stores inside functions are removed
        // Function structure: push sp, push ra, code, pop ra, pop sp, j ra
        let input = vec![
            InstructionNode::new(Instruction::Push(Operand::Register(17)), None),
            InstructionNode::new(Instruction::Push(Operand::Register(16)), None),
            InstructionNode::new(
                Instruction::Move(Operand::Register(1), Operand::Number(5.into())),
                None,
            ),
            // r1 is never read, so the move above should be dead
            InstructionNode::new(
                Instruction::Move(Operand::Register(15), Operand::Number(42.into())),
                None,
            ),
            InstructionNode::new(Instruction::Pop(Operand::Register(16)), None),
            InstructionNode::new(Instruction::Pop(Operand::Register(17)), None),
            InstructionNode::new(Instruction::Jump(Operand::Register(16)), None),
        ];

        let (output, changed) = dead_store_elimination(input);
        assert!(changed, "Dead store should be detected");
        // Should remove the move r1 5 (index 2) and move r15 42 (index 3) since neither is read
        assert_eq!(output.len(), 5);
    }
}
