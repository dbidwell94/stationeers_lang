use il::{Instruction, InstructionNode, Operand};
use rust_decimal::Decimal;
use std::collections::HashMap;

/// Pass: Resolve Labels
/// Converts all Jump/Branch labels to absolute line numbers and removes LabelDefs.
pub fn resolve_labels<'a>(input: Vec<InstructionNode<'a>>) -> Vec<InstructionNode<'a>> {
    let mut label_map: HashMap<String, usize> = HashMap::new();
    let mut line_number = 0;

    // Build Label Map (filtering out LabelDefs from the count)
    for node in &input {
        if let Instruction::LabelDef(name) = &node.instruction {
            label_map.insert(name.to_string(), line_number);
        } else {
            line_number += 1;
        }
    }

    let mut output = Vec::with_capacity(input.len());

    // Rewrite Jumps and Filter Labels
    for mut node in input {
        // Helper to get line number as Decimal operand
        let get_line = |lbl: &Operand| -> Option<Operand<'a>> {
            if let Operand::Label(name) = lbl {
                label_map
                    .get(name.as_ref())
                    .map(|&l| Operand::Number(Decimal::from(l)))
            } else {
                None
            }
        };

        match &mut node.instruction {
            Instruction::LabelDef(_) => continue, // Strip labels

            // Jumps
            Instruction::Jump(op) => {
                if let Some(num) = get_line(op) {
                    *op = num;
                }
            }
            Instruction::JumpAndLink(op) => {
                if let Some(num) = get_line(op) {
                    *op = num;
                }
            }
            Instruction::BranchEq(_, _, op)
            | Instruction::BranchNe(_, _, op)
            | Instruction::BranchGt(_, _, op)
            | Instruction::BranchLt(_, _, op)
            | Instruction::BranchGe(_, _, op)
            | Instruction::BranchLe(_, _, op) => {
                if let Some(num) = get_line(op) {
                    *op = num;
                }
            }
            Instruction::BranchEqZero(_, op) | Instruction::BranchNeZero(_, op) => {
                if let Some(num) = get_line(op) {
                    *op = num;
                }
            }
            _ => {}
        }
        output.push(node);
    }

    output
}
