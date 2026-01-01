use il::{Instruction, Operand};

/// Returns the register number written to by an instruction, if any.
pub fn get_destination_reg(instr: &Instruction) -> Option<u8> {
    match instr {
        Instruction::Move(Operand::Register(r), _)
        | Instruction::Add(Operand::Register(r), _, _)
        | Instruction::Sub(Operand::Register(r), _, _)
        | Instruction::Mul(Operand::Register(r), _, _)
        | Instruction::Div(Operand::Register(r), _, _)
        | Instruction::Mod(Operand::Register(r), _, _)
        | Instruction::Pow(Operand::Register(r), _, _)
        | Instruction::Load(Operand::Register(r), _, _)
        | Instruction::LoadSlot(Operand::Register(r), _, _, _)
        | Instruction::LoadBatch(Operand::Register(r), _, _, _)
        | Instruction::LoadBatchNamed(Operand::Register(r), _, _, _, _)
        | Instruction::SetEq(Operand::Register(r), _, _)
        | Instruction::SetNe(Operand::Register(r), _, _)
        | Instruction::SetGt(Operand::Register(r), _, _)
        | Instruction::SetLt(Operand::Register(r), _, _)
        | Instruction::SetGe(Operand::Register(r), _, _)
        | Instruction::SetLe(Operand::Register(r), _, _)
        | Instruction::And(Operand::Register(r), _, _)
        | Instruction::Or(Operand::Register(r), _, _)
        | Instruction::Xor(Operand::Register(r), _, _)
        | Instruction::Peek(Operand::Register(r))
        | Instruction::Get(Operand::Register(r), _, _)
        | Instruction::Select(Operand::Register(r), _, _, _)
        | Instruction::Rand(Operand::Register(r))
        | Instruction::Acos(Operand::Register(r), _)
        | Instruction::Asin(Operand::Register(r), _)
        | Instruction::Atan(Operand::Register(r), _)
        | Instruction::Atan2(Operand::Register(r), _, _)
        | Instruction::Abs(Operand::Register(r), _)
        | Instruction::Ceil(Operand::Register(r), _)
        | Instruction::Cos(Operand::Register(r), _)
        | Instruction::Floor(Operand::Register(r), _)
        | Instruction::Log(Operand::Register(r), _)
        | Instruction::Max(Operand::Register(r), _, _)
        | Instruction::Min(Operand::Register(r), _, _)
        | Instruction::Sin(Operand::Register(r), _)
        | Instruction::Sqrt(Operand::Register(r), _)
        | Instruction::Tan(Operand::Register(r), _)
        | Instruction::Trunc(Operand::Register(r), _)
        | Instruction::LoadReagent(Operand::Register(r), _, _, _)
        | Instruction::Pop(Operand::Register(r)) => Some(*r),
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
        Instruction::LoadReagent(_, b, c, d) => {
            Some(Instruction::LoadReagent(r, b.clone(), c.clone(), d.clone()))
        }
        Instruction::SetEq(_, a, b) => Some(Instruction::SetEq(r, a.clone(), b.clone())),
        Instruction::SetNe(_, a, b) => Some(Instruction::SetNe(r, a.clone(), b.clone())),
        Instruction::SetGt(_, a, b) => Some(Instruction::SetGt(r, a.clone(), b.clone())),
        Instruction::SetLt(_, a, b) => Some(Instruction::SetLt(r, a.clone(), b.clone())),
        Instruction::SetGe(_, a, b) => Some(Instruction::SetGe(r, a.clone(), b.clone())),
        Instruction::SetLe(_, a, b) => Some(Instruction::SetLe(r, a.clone(), b.clone())),
        Instruction::And(_, a, b) => Some(Instruction::And(r, a.clone(), b.clone())),
        Instruction::Or(_, a, b) => Some(Instruction::Or(r, a.clone(), b.clone())),
        Instruction::Xor(_, a, b) => Some(Instruction::Xor(r, a.clone(), b.clone())),
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
    let check = |op: &Operand| matches!(op, Operand::Register(r) if *r == reg);

    match instr {
        Instruction::Move(_, a) => check(a),
        Instruction::Add(_, a, b)
        | Instruction::Sub(_, a, b)
        | Instruction::Mul(_, a, b)
        | Instruction::Div(_, a, b)
        | Instruction::Mod(_, a, b)
        | Instruction::Pow(_, a, b) => check(a) || check(b),
        Instruction::Load(_, a, _) => check(a),
        Instruction::Store(a, _, b) => check(a) || check(b),
        Instruction::BranchEq(a, b, _)
        | Instruction::BranchNe(a, b, _)
        | Instruction::BranchGt(a, b, _)
        | Instruction::BranchLt(a, b, _)
        | Instruction::BranchGe(a, b, _)
        | Instruction::BranchLe(a, b, _) => check(a) || check(b),
        Instruction::BranchEqZero(a, _) | Instruction::BranchNeZero(a, _) => check(a),
        Instruction::LoadReagent(_, device, _, item_hash) => check(device) || check(item_hash),
        Instruction::LoadSlot(_, dev, slot, _) => check(dev) || check(slot),
        Instruction::LoadBatch(_, dev, _, mode) => check(dev) || check(mode),
        Instruction::LoadBatchNamed(_, d_hash, n_hash, _, mode) => {
            check(d_hash) || check(n_hash) || check(mode)
        }
        Instruction::SetEq(_, a, b)
        | Instruction::SetNe(_, a, b)
        | Instruction::SetGt(_, a, b)
        | Instruction::SetLt(_, a, b)
        | Instruction::SetGe(_, a, b)
        | Instruction::SetLe(_, a, b)
        | Instruction::And(_, a, b)
        | Instruction::Or(_, a, b)
        | Instruction::Xor(_, a, b) => check(a) || check(b),
        Instruction::Push(a) => check(a),
        Instruction::Get(_, a, b) => check(a) || check(b),
        Instruction::Put(a, b, c) => check(a) || check(b) || check(c),
        Instruction::Select(_, a, b, c) => check(a) || check(b) || check(c),
        Instruction::Sleep(a) => check(a),
        Instruction::Acos(_, a)
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
        | Instruction::Trunc(_, a) => check(a),
        Instruction::Atan2(_, a, b) | Instruction::Max(_, a, b) | Instruction::Min(_, a, b) => {
            check(a) || check(b)
        }
        _ => false,
    }
}
