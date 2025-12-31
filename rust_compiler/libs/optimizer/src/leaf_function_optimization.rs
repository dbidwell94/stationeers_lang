use crate::leaf_function::find_leaf_functions;
use il::{Instruction, InstructionNode, Operand};
use rust_decimal::Decimal;
use std::collections::{HashMap, HashSet};

/// Helper: Check if a function body contains unsafe stack manipulation.
fn function_has_complex_stack_ops(
    instructions: &[InstructionNode],
    start_idx: usize,
    end_idx: usize,
) -> bool {
    for instruction in instructions.iter().take(end_idx).skip(start_idx) {
        match instruction.instruction {
            Instruction::Push(_) | Instruction::Pop(_) => return true,
            Instruction::Add(Operand::StackPointer, _, _)
            | Instruction::Sub(Operand::StackPointer, _, _)
            | Instruction::Mul(Operand::StackPointer, _, _)
            | Instruction::Div(Operand::StackPointer, _, _)
            | Instruction::Move(Operand::StackPointer, _) => return true,
            _ => {}
        }
    }
    false
}

/// Pass: Leaf Function Optimization
/// If a function makes no calls (is a leaf), it doesn't need to save/restore `ra`.
pub fn optimize_leaf_functions<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let leaves = find_leaf_functions(&input);
    if leaves.is_empty() {
        return (input, false);
    }

    let mut changed = false;
    let mut to_remove = HashSet::new();
    let mut func_restore_indices = HashMap::new();
    let mut func_ra_offsets = HashMap::new();
    let mut current_function: Option<String> = None;
    let mut function_start_indices = HashMap::new();

    // First scan: Identify instructions to remove and capture RA offsets
    for (i, node) in input.iter().enumerate() {
        match &node.instruction {
            Instruction::LabelDef(label) if !label.starts_with("__internal_L") => {
                current_function = Some(label.to_string());
                function_start_indices.insert(label.to_string(), i);
            }
            Instruction::Push(Operand::ReturnAddress) => {
                if let Some(func) = &current_function
                    && leaves.contains(func)
                {
                    to_remove.insert(i);
                }
            }
            Instruction::Get(Operand::ReturnAddress, _, Operand::Register(_)) => {
                if let Some(func) = &current_function
                    && leaves.contains(func)
                {
                    to_remove.insert(i);
                    func_restore_indices.insert(func.clone(), i);

                    // Look back for the address calc: `sub r0 sp OFFSET`
                    if i > 0
                        && let Instruction::Sub(_, Operand::StackPointer, Operand::Number(n)) =
                            &input[i - 1].instruction
                    {
                        func_ra_offsets.insert(func.clone(), *n);
                        to_remove.insert(i - 1);
                    }
                }
            }
            _ => {}
        }
    }

    // Safety Check: Verify functions don't have complex stack ops
    let mut safe_functions = HashSet::new();

    for (func, start_idx) in &function_start_indices {
        if let Some(restore_idx) = func_restore_indices.get(func) {
            let check_start = if to_remove.contains(&(start_idx + 1)) {
                start_idx + 2
            } else {
                start_idx + 1
            };

            if !function_has_complex_stack_ops(&input, check_start, *restore_idx) {
                safe_functions.insert(func.clone());
                changed = true;
            }
        }
    }

    if !changed {
        return (input, false);
    }

    // Second scan: Rebuild with adjustments
    let mut output = Vec::with_capacity(input.len());
    let mut processing_function: Option<String> = None;

    for (i, mut node) in input.into_iter().enumerate() {
        if to_remove.contains(&i)
            && let Some(func) = &processing_function
            && safe_functions.contains(func)
        {
            continue;
        }

        if let Instruction::LabelDef(l) = &node.instruction
            && !l.starts_with("__internal_L")
        {
            processing_function = Some(l.to_string());
        }

        // Apply Stack Adjustments
        if let Some(func) = &processing_function
            && safe_functions.contains(func)
            && let Some(ra_offset) = func_ra_offsets.get(func)
        {
            // Stack Cleanup Adjustment
            if let Instruction::Sub(
                Operand::StackPointer,
                Operand::StackPointer,
                Operand::Number(n),
            ) = &mut node.instruction
            {
                let new_n = *n - Decimal::from(1);
                if new_n.is_zero() {
                    continue;
                }
                *n = new_n;
            }

            // Stack Variable Offset Adjustment
            if let Instruction::Sub(_, Operand::StackPointer, Operand::Number(n)) =
                &mut node.instruction
                && *n > *ra_offset
            {
                *n -= Decimal::from(1);
            }
        }

        output.push(node);
    }

    (output, true)
}
