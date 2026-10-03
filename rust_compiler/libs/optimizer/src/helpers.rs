use il::{DeviceReference, Instruction, LiteralOrReference, Operand};

/// Returns the register number written to by an instruction, if any.
pub fn get_destination_reg(instr: &Instruction) -> Option<u8> {
    match get_destination_operand(instr)? {
        Operand::Register(reg) => Some(*reg),
        _ => None,
    }
}

/// Returns the virtual register written to by an instruction, if any.
#[allow(dead_code)]
pub fn get_virtual_destination_reg(instr: &Instruction) -> Option<u32> {
    match get_destination_operand(instr)? {
        Operand::VirtualRegister(reg) => Some(*reg),
        _ => None,
    }
}

fn get_destination_operand<'a, 'b>(instr: &'a Instruction<'b>) -> Option<&'a Operand<'b>> {
    match instr {
        Instruction::Move(dst, _)
        | Instruction::Add(dst, _, _)
        | Instruction::Sub(dst, _, _)
        | Instruction::Mul(dst, _, _)
        | Instruction::Div(dst, _, _)
        | Instruction::Mod(dst, _, _)
        | Instruction::Pow(dst, _, _)
        | Instruction::Sll(dst, _, _)
        | Instruction::Sra(dst, _, _)
        | Instruction::Srl(dst, _, _)
        | Instruction::Load(dst, _, _)
        | Instruction::LoadSlot(dst, _, _, _)
        | Instruction::LoadBatch(dst, _, _, _)
        | Instruction::LoadBatchNamed(dst, _, _, _, _)
        | Instruction::LoadBatchSlot(dst, _, _, _, _)
        | Instruction::LoadBatchNamedSlot(dst, _, _, _, _, _)
        | Instruction::LoadReagent(dst, _, _, _)
        | Instruction::DeviceSet(dst, _)
        | Instruction::DeviceNotSet(dst, _)
        | Instruction::Rmap(dst, _, _)
        | Instruction::SetEq(dst, _, _)
        | Instruction::SetNe(dst, _, _)
        | Instruction::SetGt(dst, _, _)
        | Instruction::SetLt(dst, _, _)
        | Instruction::SetGe(dst, _, _)
        | Instruction::SetLe(dst, _, _)
        | Instruction::And(dst, _, _)
        | Instruction::Or(dst, _, _)
        | Instruction::Xor(dst, _, _)
        | Instruction::Nor(dst, _, _)
        | Instruction::Not(dst, _)
        | Instruction::Peek(dst)
        | Instruction::Get(dst, _, _)
        | Instruction::Select(dst, _, _, _)
        | Instruction::Rand(dst)
        | Instruction::Acos(dst, _)
        | Instruction::Asin(dst, _)
        | Instruction::Atan(dst, _)
        | Instruction::Atan2(dst, _, _)
        | Instruction::Abs(dst, _)
        | Instruction::Ceil(dst, _)
        | Instruction::Cos(dst, _)
        | Instruction::Floor(dst, _)
        | Instruction::Log(dst, _)
        | Instruction::Max(dst, _, _)
        | Instruction::Min(dst, _, _)
        | Instruction::Sin(dst, _)
        | Instruction::Sqrt(dst, _)
        | Instruction::Tan(dst, _)
        | Instruction::Trunc(dst, _)
        | Instruction::Pop(dst) => Some(dst),
        _ => None,
    }
}

/// Creates a new instruction with the destination register changed.
pub fn set_destination_reg<'a>(instr: &Instruction<'a>, new_reg: u8) -> Option<Instruction<'a>> {
    let r = Operand::Register(new_reg);
    match instr {
        Instruction::Move(_, b) => Some(Instruction::Move(r, b.clone())),
        Instruction::Add(_, a, b) => Some(Instruction::Add(r, a.clone(), b.clone())),
        Instruction::Sub(_, a, b) => Some(Instruction::Sub(r, a.clone(), b.clone())),
        Instruction::Mul(_, a, b) => Some(Instruction::Mul(r, a.clone(), b.clone())),
        Instruction::Div(_, a, b) => Some(Instruction::Div(r, a.clone(), b.clone())),
        Instruction::Mod(_, a, b) => Some(Instruction::Mod(r, a.clone(), b.clone())),
        Instruction::Pow(_, a, b) => Some(Instruction::Pow(r, a.clone(), b.clone())),
        Instruction::Sll(_, a, b) => Some(Instruction::Sll(r, a.clone(), b.clone())),
        Instruction::Sra(_, a, b) => Some(Instruction::Sra(r, a.clone(), b.clone())),
        Instruction::Srl(_, a, b) => Some(Instruction::Srl(r, a.clone(), b.clone())),
        Instruction::Load(_, a, b) => Some(Instruction::Load(r, a.clone(), b.clone())),
        Instruction::LoadSlot(_, a, b, c) => {
            Some(Instruction::LoadSlot(r, a.clone(), b.clone(), c.clone()))
        }
        Instruction::LoadBatch(_, a, b, c) => {
            Some(Instruction::LoadBatch(r, a.clone(), b.clone(), c.clone()))
        }
        Instruction::LoadBatchNamed(_, a, b, c, d) => Some(Instruction::LoadBatchNamed(
            r,
            a.clone(),
            b.clone(),
            c.clone(),
            d.clone(),
        )),
        Instruction::LoadBatchSlot(_, a, b, c, d) => Some(Instruction::LoadBatchSlot(
            r,
            a.clone(),
            b.clone(),
            c.clone(),
            d.clone(),
        )),
        Instruction::LoadBatchNamedSlot(_, a, b, c, d, e) => Some(Instruction::LoadBatchNamedSlot(
            r,
            a.clone(),
            b.clone(),
            c.clone(),
            d.clone(),
            e.clone(),
        )),
        Instruction::LoadReagent(_, b, c, d) => {
            Some(Instruction::LoadReagent(r, b.clone(), c.clone(), d.clone()))
        }
        Instruction::DeviceSet(_, a) => Some(Instruction::DeviceSet(r, a.clone())),
        Instruction::DeviceNotSet(_, a) => Some(Instruction::DeviceNotSet(r, a.clone())),
        Instruction::Rmap(_, a, b) => Some(Instruction::Rmap(r, a.clone(), b.clone())),
        Instruction::SetEq(_, a, b) => Some(Instruction::SetEq(r, a.clone(), b.clone())),
        Instruction::SetNe(_, a, b) => Some(Instruction::SetNe(r, a.clone(), b.clone())),
        Instruction::SetGt(_, a, b) => Some(Instruction::SetGt(r, a.clone(), b.clone())),
        Instruction::SetLt(_, a, b) => Some(Instruction::SetLt(r, a.clone(), b.clone())),
        Instruction::SetGe(_, a, b) => Some(Instruction::SetGe(r, a.clone(), b.clone())),
        Instruction::SetLe(_, a, b) => Some(Instruction::SetLe(r, a.clone(), b.clone())),
        Instruction::And(_, a, b) => Some(Instruction::And(r, a.clone(), b.clone())),
        Instruction::Or(_, a, b) => Some(Instruction::Or(r, a.clone(), b.clone())),
        Instruction::Xor(_, a, b) => Some(Instruction::Xor(r, a.clone(), b.clone())),
        Instruction::Nor(_, a, b) => Some(Instruction::Nor(r, a.clone(), b.clone())),
        Instruction::Not(_, a) => Some(Instruction::Not(r, a.clone())),
        Instruction::Peek(_) => Some(Instruction::Peek(r)),
        Instruction::Get(_, a, b) => Some(Instruction::Get(r, a.clone(), b.clone())),
        Instruction::Select(_, a, b, c) => {
            Some(Instruction::Select(r, a.clone(), b.clone(), c.clone()))
        }
        Instruction::Rand(_) => Some(Instruction::Rand(r)),
        Instruction::Pop(_) => Some(Instruction::Pop(r)),
        Instruction::Acos(_, a) => Some(Instruction::Acos(r, a.clone())),
        Instruction::Asin(_, a) => Some(Instruction::Asin(r, a.clone())),
        Instruction::Atan(_, a) => Some(Instruction::Atan(r, a.clone())),
        Instruction::Atan2(_, a, b) => Some(Instruction::Atan2(r, a.clone(), b.clone())),
        Instruction::Abs(_, a) => Some(Instruction::Abs(r, a.clone())),
        Instruction::Ceil(_, a) => Some(Instruction::Ceil(r, a.clone())),
        Instruction::Cos(_, a) => Some(Instruction::Cos(r, a.clone())),
        Instruction::Floor(_, a) => Some(Instruction::Floor(r, a.clone())),
        Instruction::Log(_, a) => Some(Instruction::Log(r, a.clone())),
        Instruction::Max(_, a, b) => Some(Instruction::Max(r, a.clone(), b.clone())),
        Instruction::Min(_, a, b) => Some(Instruction::Min(r, a.clone(), b.clone())),
        Instruction::Sin(_, a) => Some(Instruction::Sin(r, a.clone())),
        Instruction::Sqrt(_, a) => Some(Instruction::Sqrt(r, a.clone())),
        Instruction::Tan(_, a) => Some(Instruction::Tan(r, a.clone())),
        Instruction::Trunc(_, a) => Some(Instruction::Trunc(r, a.clone())),
        _ => None,
    }
}

/// Checks if a register is read by an instruction.
pub fn reg_is_read(instr: &Instruction, reg: u8) -> bool {
    instruction_reads(instr, |op| match op {
        Operand::Register(register) => *register == reg,
        Operand::DeviceReference(
            DeviceReference::Housing(LiteralOrReference::Reference(register))
            | DeviceReference::Pin(LiteralOrReference::Reference(register))
            | DeviceReference::Reference(LiteralOrReference::Reference(register)),
        ) => *register == reg,
        _ => false,
    })
}

/// Checks if a virtual register is read by an instruction.
#[allow(dead_code)]
pub fn virtual_reg_is_read(instr: &Instruction, reg: u32) -> bool {
    instruction_reads(instr, |op| {
        matches!(op, Operand::VirtualRegister(register) if *register == reg)
            || matches!(
                op,
                Operand::DeviceReference(
                    DeviceReference::Housing(LiteralOrReference::VirtualReference(register))
                    | DeviceReference::Pin(LiteralOrReference::VirtualReference(register))
                    | DeviceReference::Reference(LiteralOrReference::VirtualReference(register))
                ) if *register == reg
            )
    })
}

fn instruction_reads(instr: &Instruction, check: impl Fn(&Operand) -> bool) -> bool {
    match instr {
        Instruction::Move(_, a)
        | Instruction::Acos(_, a)
        | Instruction::Asin(_, a)
        | Instruction::Atan(_, a)
        | Instruction::Abs(_, a)
        | Instruction::Ceil(_, a)
        | Instruction::Cos(_, a)
        | Instruction::Floor(_, a)
        | Instruction::Log(_, a)
        | Instruction::Sin(_, a)
        | Instruction::Sqrt(_, a)
        | Instruction::Tan(_, a)
        | Instruction::Trunc(_, a)
        | Instruction::Not(_, a)
        | Instruction::Push(a)
        | Instruction::Sleep(a)
        | Instruction::Jump(a)
        | Instruction::JumpAndLink(a)
        | Instruction::JumpRelative(a)
        | Instruction::Clr(a)
        | Instruction::DeviceSet(_, a)
        | Instruction::DeviceNotSet(_, a)
        | Instruction::Alias(_, a) => check(a),

        Instruction::Add(_, a, b)
        | Instruction::Sub(_, a, b)
        | Instruction::Mul(_, a, b)
        | Instruction::Div(_, a, b)
        | Instruction::Mod(_, a, b)
        | Instruction::Pow(_, a, b)
        | Instruction::Sll(_, a, b)
        | Instruction::Sra(_, a, b)
        | Instruction::Srl(_, a, b)
        | Instruction::Atan2(_, a, b)
        | Instruction::Max(_, a, b)
        | Instruction::Min(_, a, b)
        | Instruction::SetEq(_, a, b)
        | Instruction::SetNe(_, a, b)
        | Instruction::SetGt(_, a, b)
        | Instruction::SetLt(_, a, b)
        | Instruction::SetGe(_, a, b)
        | Instruction::SetLe(_, a, b)
        | Instruction::And(_, a, b)
        | Instruction::Or(_, a, b)
        | Instruction::Xor(_, a, b)
        | Instruction::Nor(_, a, b)
        | Instruction::Get(_, a, b)
        | Instruction::Rmap(_, a, b)
        | Instruction::Load(_, a, b)
        | Instruction::BranchEq(a, b, _)
        | Instruction::BranchNe(a, b, _)
        | Instruction::BranchGt(a, b, _)
        | Instruction::BranchLt(a, b, _)
        | Instruction::BranchGe(a, b, _)
        | Instruction::BranchLe(a, b, _)
        | Instruction::BranchEqZero(a, b)
        | Instruction::BranchNeZero(a, b) => check(a) || check(b),

        Instruction::Store(a, b, c)
        | Instruction::StoreBatch(a, b, c)
        | Instruction::LoadSlot(_, a, b, c)
        | Instruction::LoadBatch(_, a, b, c)
        | Instruction::LoadReagent(_, a, b, c)
        | Instruction::Put(a, b, c)
        | Instruction::Select(_, a, b, c) => check(a) || check(b) || check(c),

        Instruction::StoreSlot(a, b, c, d)
        | Instruction::StoreBatchNamed(a, b, c, d)
        | Instruction::LoadBatchNamed(_, a, b, c, d)
        | Instruction::LoadBatchSlot(_, a, b, c, d) => check(a) || check(b) || check(c) || check(d),

        Instruction::LoadBatchNamedSlot(_, a, b, c, d, e) => {
            check(a) || check(b) || check(c) || check(d) || check(e)
        }

        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indirect_pin_operand_reads_its_reference_register() {
        let instruction = Instruction::Load(
            Operand::Register(1),
            Operand::DeviceReference(DeviceReference::Pin(LiteralOrReference::Reference(8))),
            Operand::LogicType("On".into()),
        );

        assert!(reg_is_read(&instruction, 8));
    }

    #[test]
    fn indirect_ref_id_operand_reads_its_reference_register() {
        let instruction = Instruction::Store(
            Operand::DeviceReference(DeviceReference::Reference(LiteralOrReference::Reference(8))),
            Operand::LogicType("On".into()),
            Operand::Number(1.into()),
        );

        assert!(reg_is_read(&instruction, 8));
    }

    #[test]
    fn literal_device_reference_does_not_read_a_register() {
        let instruction = Instruction::Load(
            Operand::Register(1),
            Operand::DeviceReference(DeviceReference::Pin(LiteralOrReference::Literal(0.into()))),
            Operand::LogicType("On".into()),
        );

        assert!(!reg_is_read(&instruction, 8));
    }

    #[test]
    fn virtual_register_access_tracks_destination_and_sources() {
        let instruction = Instruction::Add(
            Operand::VirtualRegister(12),
            Operand::VirtualRegister(5),
            Operand::Register(2),
        );

        assert_eq!(get_virtual_destination_reg(&instruction), Some(12));
        assert!(virtual_reg_is_read(&instruction, 5));
        assert!(!virtual_reg_is_read(&instruction, 12));
    }
}
