use crate::helpers::get_destination_reg;
use il::{Instruction, InstructionNode, Operand};
use rust_decimal::Decimal;

/// Pass: Constant Propagation
/// Folds arithmetic operations when both operands are constant.
/// Also tracks register values and propagates them forward.
pub fn constant_propagation<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    let mut registers: [Option<Decimal>; 16] = [None; 16];

    for mut node in input {
        // Invalidate register tracking on label/call boundaries
        match &node.instruction {
            Instruction::LabelDef(_) | Instruction::JumpAndLink(_) => registers = [None; 16],
            _ => {}
        }

        let simplified = match &node.instruction {
            Instruction::Move(dst, src) => resolve_value(src, &registers)
                .map(|val| Instruction::Move(dst.clone(), Operand::Number(val))),
            Instruction::Add(dst, a, b) => try_fold_math(dst, a, b, &registers, |x, y| x + y),
            Instruction::Sub(dst, a, b) => try_fold_math(dst, a, b, &registers, |x, y| x - y),
            Instruction::Mul(dst, a, b) => try_fold_math(dst, a, b, &registers, |x, y| x * y),
            Instruction::Div(dst, a, b) => try_fold_math(dst, a, b, &registers, |x, y| {
                if y.is_zero() { Decimal::ZERO } else { x / y }
            }),
            Instruction::Mod(dst, a, b) => try_fold_math(dst, a, b, &registers, |x, y| {
                if y.is_zero() { Decimal::ZERO } else { x % y }
            }),
            Instruction::BranchEq(a, b, l) => {
                try_resolve_branch(a, b, l, &registers, |x, y| x == y)
            }
            Instruction::BranchNe(a, b, l) => {
                try_resolve_branch(a, b, l, &registers, |x, y| x != y)
            }
            Instruction::BranchGt(a, b, l) => try_resolve_branch(a, b, l, &registers, |x, y| x > y),
            Instruction::BranchLt(a, b, l) => try_resolve_branch(a, b, l, &registers, |x, y| x < y),
            Instruction::BranchGe(a, b, l) => {
                try_resolve_branch(a, b, l, &registers, |x, y| x >= y)
            }
            Instruction::BranchLe(a, b, l) => {
                try_resolve_branch(a, b, l, &registers, |x, y| x <= y)
            }
            Instruction::BranchEqZero(a, l) => {
                try_resolve_branch(a, &Operand::Number(0.into()), l, &registers, |x, y| x == y)
            }
            Instruction::BranchNeZero(a, l) => {
                try_resolve_branch(a, &Operand::Number(0.into()), l, &registers, |x, y| x != y)
            }
            _ => None,
        };

        if let Some(new) = simplified {
            node.instruction = new;
            changed = true;
        }

        // Update register tracking
        match &node.instruction {
            Instruction::Move(Operand::Register(r), src) => {
                registers[*r as usize] = resolve_value(src, &registers)
            }
            _ => {
                if let Some(r) = get_destination_reg(&node.instruction) {
                    registers[r as usize] = None;
                }
            }
        }

        // Filter out NOPs (empty labels from branch resolution)
        if let Instruction::LabelDef(l) = &node.instruction
            && l.is_empty()
        {
            changed = true;
            continue;
        }

        output.push(node);
    }
    (output, changed)
}

fn resolve_value(op: &Operand, regs: &[Option<Decimal>; 16]) -> Option<Decimal> {
    match op {
        Operand::Number(n) => Some(*n),
        Operand::Register(r) => regs[*r as usize],
        _ => None,
    }
}

fn try_fold_math<'a, F>(
    dst: &Operand<'a>,
    a: &Operand<'a>,
    b: &Operand<'a>,
    regs: &[Option<Decimal>; 16],
    op: F,
) -> Option<Instruction<'a>>
where
    F: Fn(Decimal, Decimal) -> Decimal,
{
    let val_a = resolve_value(a, regs)?;
    let val_b = resolve_value(b, regs)?;
    Some(Instruction::Move(
        dst.clone(),
        Operand::Number(op(val_a, val_b)),
    ))
}

fn try_resolve_branch<'a, F>(
    a: &Operand<'a>,
    b: &Operand<'a>,
    label: &Operand<'a>,
    regs: &[Option<Decimal>; 16],
    check: F,
) -> Option<Instruction<'a>>
where
    F: Fn(Decimal, Decimal) -> bool,
{
    let val_a = resolve_value(a, regs)?;
    let val_b = resolve_value(b, regs)?;
    if check(val_a, val_b) {
        Some(Instruction::Jump(label.clone()))
    } else {
        Some(Instruction::LabelDef("".into())) // NOP
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use il::InstructionNode;

    #[test]
    fn test_fold_add() {
        let input = vec![InstructionNode::new(
            Instruction::Add(
                Operand::Register(1),
                Operand::Number(5.into()),
                Operand::Number(3.into()),
            ),
            None,
        )];

        let (output, changed) = constant_propagation(input);
        assert!(changed);
        assert_eq!(output.len(), 1);
        assert!(matches!(
            output[0].instruction,
            Instruction::Move(Operand::Register(1), Operand::Number(_))
        ));
    }
}
