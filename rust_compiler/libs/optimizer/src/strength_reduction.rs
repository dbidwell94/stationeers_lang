use il::{Instruction, InstructionNode, Operand};
use rust_decimal::Decimal;

/// Pass: Strength Reduction
/// Replaces expensive operations with cheaper equivalents.
/// Example: x * 2 -> add x x x (addition is typically faster than multiplication)
pub fn strength_reduction<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;

    for mut node in input {
        let reduced = match &node.instruction {
            // x * 2 = x + x
            Instruction::Mul(dst, a, Operand::Number(n)) if *n == Decimal::from(2) => {
                Some(Instruction::Add(dst.clone(), a.clone(), a.clone()))
            }
            Instruction::Mul(dst, Operand::Number(n), b) if *n == Decimal::from(2) => {
                Some(Instruction::Add(dst.clone(), b.clone(), b.clone()))
            }

            // Future: Could add power-of-2 optimizations using bit shifts if IC10 supports them
            // x * 4 = (x + x) + (x + x) or x << 2
            // x / 2 = x >> 1

            _ => None,
        };

        if let Some(new) = reduced {
            node.instruction = new;
            changed = true;
        }

        output.push(node);
    }

    (output, changed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mul_two_to_add() {
        let input = vec![InstructionNode::new(
            Instruction::Mul(
                Operand::Register(1),
                Operand::Register(2),
                Operand::Number(Decimal::from(2)),
            ),
            None,
        )];

        let (output, changed) = strength_reduction(input);
        assert!(changed);
        assert!(matches!(
            output[0].instruction,
            Instruction::Add(Operand::Register(1), Operand::Register(2), Operand::Register(2))
        ));
    }
}
