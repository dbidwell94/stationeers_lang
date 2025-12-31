use il::{Instruction, InstructionNode};

/// Pass: Redundant Move Elimination  
/// Removes moves where source and destination are the same: `move rx rx`
pub fn remove_redundant_moves<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    for node in input {
        if let Instruction::Move(dst, src) = &node.instruction
            && dst == src
        {
            changed = true;
            continue;
        }
        output.push(node);
    }
    (output, changed)
}

/// Pass: Dead Code Elimination
/// Removes unreachable code after unconditional jumps.
pub fn remove_unreachable_code<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    let mut dead = false;
    for node in input {
        if let Instruction::LabelDef(_) = node.instruction {
            dead = false;
        }
        if dead {
            changed = true;
            continue;
        }
        if let Instruction::Jump(_) = node.instruction {
            dead = true
        }
        output.push(node);
    }
    (output, changed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use il::{Instruction, InstructionNode, Operand};

    #[test]
    fn test_remove_redundant_move() {
        let input = vec![InstructionNode::new(
            Instruction::Move(Operand::Register(1), Operand::Register(1)),
            None,
        )];

        let (output, changed) = remove_redundant_moves(input);
        assert!(changed);
        assert_eq!(output.len(), 0);
    }

    #[test]
    fn test_remove_unreachable() {
        let input = vec![
            InstructionNode::new(Instruction::Jump(Operand::Label("main".into())), None),
            InstructionNode::new(
                Instruction::Add(
                    Operand::Register(1),
                    Operand::Number(1.into()),
                    Operand::Number(2.into()),
                ),
                None,
            ),
            InstructionNode::new(Instruction::LabelDef("main".into()), None),
        ];

        let (output, changed) = remove_unreachable_code(input);
        assert!(changed);
        assert_eq!(output.len(), 2);
    }
}
