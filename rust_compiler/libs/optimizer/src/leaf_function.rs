use il::{Instruction, InstructionNode};
use std::collections::HashSet;

/// Scans the instruction set to identify "leaf functions".
/// A leaf function is defined as a function (delimited by LabelDefs) that does not
/// contain any `jal` (JumpAndLink) instructions.
///
/// Returns a Set containing the names of all identified leaf functions.
pub fn find_leaf_functions(instructions: &[InstructionNode]) -> HashSet<String> {
    let mut leaf_functions = HashSet::new();
    let mut current_label: Option<String> = None;
    let mut is_current_leaf = true;

    for node in instructions {
        match &node.instruction {
            Instruction::LabelDef(label) => {
                if label.starts_with("__internal_L") {
                    continue;
                }

                // If we were tracking a function, and it remained a leaf until now, save it.
                if let Some(name) = current_label.take()
                    && is_current_leaf
                {
                    leaf_functions.insert(name);
                }

                // Start tracking the new function
                current_label = Some(label.to_string());
                is_current_leaf = true;
            }
            Instruction::JumpAndLink(_) => {
                // If we see a JAL, this function is NOT a leaf.
                is_current_leaf = false;
            }
            _ => {}
        }
    }

    // Handle the final function in the file
    if let Some(name) = current_label
        && is_current_leaf
    {
        leaf_functions.insert(name);
    }

    leaf_functions
}
