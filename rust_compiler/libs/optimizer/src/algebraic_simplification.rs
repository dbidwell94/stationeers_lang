use il::{Instruction, InstructionNode, Operand};
use rust_decimal::Decimal;

/// Pass: Algebraic Simplification
/// Simplifies mathematical identities like x+0, x*1, x*0, etc.
pub fn algebraic_simplification<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;

    for mut node in input {
        let simplified = match &node.instruction {
            // x + 0 = x
            Instruction::Add(dst, a, Operand::Number(n)) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), a.clone()))
            }
            Instruction::Add(dst, Operand::Number(n), b) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), b.clone()))
            }

            // x - 0 = x
            Instruction::Sub(dst, a, Operand::Number(n)) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), a.clone()))
            }

            // x * 1 = x
            Instruction::Mul(dst, a, Operand::Number(n)) if *n == Decimal::from(1) => {
                Some(Instruction::Move(dst.clone(), a.clone()))
            }
            Instruction::Mul(dst, Operand::Number(n), b) if *n == Decimal::from(1) => {
                Some(Instruction::Move(dst.clone(), b.clone()))
            }

            // x * 0 = 0
            Instruction::Mul(dst, _, Operand::Number(n)) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), Operand::Number(Decimal::ZERO)))
            }
            Instruction::Mul(dst, Operand::Number(n), _) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), Operand::Number(Decimal::ZERO)))
            }

            // x / 1 = x
            Instruction::Div(dst, a, Operand::Number(n)) if *n == Decimal::from(1) => {
                Some(Instruction::Move(dst.clone(), a.clone()))
            }

            // 0 / x = 0 (if x != 0, but we can't check at compile time for non-literals)
            Instruction::Div(dst, Operand::Number(n), _) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), Operand::Number(Decimal::ZERO)))
            }

            // x % 1 = 0
            Instruction::Mod(dst, _, Operand::Number(n)) if *n == Decimal::from(1) => {
                Some(Instruction::Move(dst.clone(), Operand::Number(Decimal::ZERO)))
            }

            // 0 % x = 0
            Instruction::Mod(dst, Operand::Number(n), _) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), Operand::Number(Decimal::ZERO)))
            }

            // x AND 0 = 0
            Instruction::And(dst, _, Operand::Number(n)) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), Operand::Number(Decimal::ZERO)))
            }
            Instruction::And(dst, Operand::Number(n), _) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), Operand::Number(Decimal::ZERO)))
            }

            // x OR 0 = x
            Instruction::Or(dst, a, Operand::Number(n)) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), a.clone()))
            }
            Instruction::Or(dst, Operand::Number(n), b) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), b.clone()))
            }

            // x XOR 0 = x
            Instruction::Xor(dst, a, Operand::Number(n)) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), a.clone()))
            }
            Instruction::Xor(dst, Operand::Number(n), b) if n.is_zero() => {
                Some(Instruction::Move(dst.clone(), b.clone()))
            }

            _ => None,
        };

        if let Some(new) = simplified {
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
    fn test_add_zero() {
        let input = vec![InstructionNode::new(
            Instruction::Add(
                Operand::Register(1),
                Operand::Register(2),
                Operand::Number(Decimal::ZERO),
            ),
            None,
        )];

        let (output, changed) = algebraic_simplification(input);
        assert!(changed);
        assert!(matches!(
            output[0].instruction,
            Instruction::Move(Operand::Register(1), Operand::Register(2))
        ));
    }

    #[test]
    fn test_mul_one() {
        let input = vec![InstructionNode::new(
            Instruction::Mul(
                Operand::Register(3),
                Operand::Register(4),
                Operand::Number(Decimal::ONE),
            ),
            None,
        )];

        let (output, changed) = algebraic_simplification(input);
        assert!(changed);
        assert!(matches!(
            output[0].instruction,
            Instruction::Move(Operand::Register(3), Operand::Register(4))
        ));
    }

    #[test]
    fn test_mul_zero() {
        let input = vec![InstructionNode::new(
            Instruction::Mul(
                Operand::Register(5),
                Operand::Register(6),
                Operand::Number(Decimal::ZERO),
            ),
            None,
        )];

        let (output, changed) = algebraic_simplification(input);
        assert!(changed);
        assert!(matches!(
            output[0].instruction,
            Instruction::Move(Operand::Register(5), Operand::Number(_))
        ));
    }
}
