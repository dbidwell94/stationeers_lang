use il::{DeviceReference, Instruction, Instructions, LiteralOrReference, Operand};
use std::io::{BufWriter, Write};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Cannot emit unallocated virtual register v{0} as IC10.")]
    VirtualRegister(u32),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

/// Formats an instruction stream as IC10, rejecting virtual registers before writing anything.
pub fn write<W: Write>(
    instructions: Instructions<'_>,
    writer: &mut BufWriter<W>,
) -> Result<(), Error> {
    let lines = instructions
        .iter()
        .map(|node| format_instruction(&node.instruction))
        .collect::<Result<Vec<_>, _>>()?;

    for line in lines {
        writer.write_all(line.as_bytes())?;
        writer.write_all(b"\n")?;
    }
    writer.flush()?;
    Ok(())
}

fn format_instruction(instruction: &Instruction<'_>) -> Result<String, Error> {
    macro_rules! line {
        ($opcode:literal $(, $operand:expr)* $(,)?) => {{
            let mut parts = vec![$opcode.to_string()];
            $(parts.push(format_operand($operand)?);)*
            Ok(parts.join(" "))
        }};
    }

    match instruction {
        Instruction::Move(dst, val) => line!("move", dst, val),
        Instruction::Add(dst, a, b) => line!("add", dst, a, b),
        Instruction::Sub(dst, a, b) => line!("sub", dst, a, b),
        Instruction::Mul(dst, a, b) => line!("mul", dst, a, b),
        Instruction::Div(dst, a, b) => line!("div", dst, a, b),
        Instruction::Mod(dst, a, b) => line!("mod", dst, a, b),
        Instruction::Pow(dst, a, b) => line!("pow", dst, a, b),
        Instruction::Acos(dst, a) => line!("acos", dst, a),
        Instruction::Asin(dst, a) => line!("asin", dst, a),
        Instruction::Atan(dst, a) => line!("atan", dst, a),
        Instruction::Atan2(dst, a, b) => line!("atan2", dst, a, b),
        Instruction::Abs(dst, a) => line!("abs", dst, a),
        Instruction::Ceil(dst, a) => line!("ceil", dst, a),
        Instruction::Cos(dst, a) => line!("cos", dst, a),
        Instruction::Floor(dst, a) => line!("floor", dst, a),
        Instruction::Log(dst, a) => line!("log", dst, a),
        Instruction::Max(dst, a, b) => line!("max", dst, a, b),
        Instruction::Min(dst, a, b) => line!("min", dst, a, b),
        Instruction::Rand(dst) => line!("rand", dst),
        Instruction::Sin(dst, a) => line!("sin", dst, a),
        Instruction::Sqrt(dst, a) => line!("sqrt", dst, a),
        Instruction::Tan(dst, a) => line!("tan", dst, a),
        Instruction::Trunc(dst, a) => line!("trunc", dst, a),
        Instruction::Load(reg, dev, typ) => line!("l", reg, dev, typ),
        Instruction::Store(dev, typ, val) => line!("s", dev, typ, val),
        Instruction::LoadSlot(reg, dev, slot, typ) => line!("ls", reg, dev, slot, typ),
        Instruction::StoreSlot(dev, slot, typ, val) => line!("ss", dev, slot, typ, val),
        Instruction::LoadBatch(reg, hash, typ, mode) => line!("lb", reg, hash, typ, mode),
        Instruction::StoreBatch(hash, typ, val) => line!("sb", hash, typ, val),
        Instruction::LoadBatchNamed(reg, d_hash, n_hash, typ, mode) => {
            line!("lbn", reg, d_hash, n_hash, typ, mode)
        }
        Instruction::StoreBatchNamed(d_hash, n_hash, typ, val) => {
            line!("sbn", d_hash, n_hash, typ, val)
        }
        Instruction::LoadBatchSlot(reg, hash, slot, typ, mode) => {
            line!("lbs", reg, hash, slot, typ, mode)
        }
        Instruction::LoadBatchNamedSlot(reg, d_hash, n_hash, slot, typ, mode) => {
            line!("lbns", reg, d_hash, n_hash, slot, typ, mode)
        }
        Instruction::LoadReagent(reg, device, reagent_mode, reagent_hash) => {
            line!("lr", reg, device, reagent_mode, reagent_hash)
        }
        Instruction::DeviceSet(reg, dev) => line!("sdse", reg, dev),
        Instruction::DeviceNotSet(reg, dev) => line!("sdns", reg, dev),
        Instruction::Rmap(reg, device, reagent_hash) => line!("rmap", reg, device, reagent_hash),
        Instruction::Jump(label) => line!("j", label),
        Instruction::JumpAndLink(label) => line!("jal", label),
        Instruction::JumpRelative(offset) => line!("jr", offset),
        Instruction::BranchEq(a, b, label) => line!("beq", a, b, label),
        Instruction::BranchNe(a, b, label) => line!("bne", a, b, label),
        Instruction::BranchGt(a, b, label) => line!("bgt", a, b, label),
        Instruction::BranchLt(a, b, label) => line!("blt", a, b, label),
        Instruction::BranchGe(a, b, label) => line!("bge", a, b, label),
        Instruction::BranchLe(a, b, label) => line!("ble", a, b, label),
        Instruction::BranchEqZero(a, label) => line!("beqz", a, label),
        Instruction::BranchNeZero(a, label) => line!("bnez", a, label),
        Instruction::SetEq(dst, a, b) => line!("seq", dst, a, b),
        Instruction::SetNe(dst, a, b) => line!("sne", dst, a, b),
        Instruction::SetGt(dst, a, b) => line!("sgt", dst, a, b),
        Instruction::SetLt(dst, a, b) => line!("slt", dst, a, b),
        Instruction::SetGe(dst, a, b) => line!("sge", dst, a, b),
        Instruction::SetLe(dst, a, b) => line!("sle", dst, a, b),
        Instruction::And(dst, a, b) => line!("and", dst, a, b),
        Instruction::Or(dst, a, b) => line!("or", dst, a, b),
        Instruction::Xor(dst, a, b) => line!("xor", dst, a, b),
        Instruction::Nor(dst, a, b) => line!("nor", dst, a, b),
        Instruction::Not(dst, a) => line!("not", dst, a),
        Instruction::Sll(dst, a, b) => line!("sll", dst, a, b),
        Instruction::Sra(dst, a, b) => line!("sra", dst, a, b),
        Instruction::Srl(dst, a, b) => line!("srl", dst, a, b),
        Instruction::Push(val) => line!("push", val),
        Instruction::Pop(dst) => line!("pop", dst),
        Instruction::Peek(dst) => line!("peek", dst),
        Instruction::Get(dst, dev, val) => line!("get", dst, dev, val),
        Instruction::Put(dev, addr, val) => line!("put", dev, addr, val),
        Instruction::Select(dst, cond, a, b) => line!("select", dst, cond, a, b),
        Instruction::Yield => Ok("yield".into()),
        Instruction::Sleep(val) => line!("sleep", val),
        Instruction::Clr(val) => line!("clr", val),
        Instruction::Alias(name, target) => Ok(format!("alias {name} {}", format_operand(target)?)),
        Instruction::Define(name, val) => Ok(format!("define {name} {val}")),
        Instruction::LabelDef(label) => Ok(format!("{label}:")),
        Instruction::Comment(text) => Ok(format!("# {text}")),
    }
}

fn format_operand(operand: &Operand<'_>) -> Result<String, Error> {
    Ok(match operand {
        Operand::Register(reg) => format!("r{reg}"),
        Operand::VirtualRegister(reg) => return Err(Error::VirtualRegister(*reg)),
        Operand::Device(device) => device.to_string(),
        Operand::DeviceReference(reference) => format_device_reference(reference)?,
        Operand::Number(number) => number.to_string(),
        Operand::Label(label) | Operand::LogicType(label) => label.to_string(),
        Operand::StackPointer => "sp".into(),
        Operand::ReturnAddress => "ra".into(),
    })
}

fn format_device_reference(reference: &DeviceReference) -> Result<String, Error> {
    Ok(match reference {
        DeviceReference::Housing(reference) | DeviceReference::Pin(reference) => match reference {
            LiteralOrReference::Literal(value) => format!("d{value}"),
            LiteralOrReference::Reference(reg) => format!("dr{reg}"),
            LiteralOrReference::VirtualReference(reg) => return Err(Error::VirtualRegister(*reg)),
        },
        DeviceReference::Reference(reference) => match reference {
            LiteralOrReference::Literal(value) => format!("${value}"),
            LiteralOrReference::Reference(reg) => format!("r{reg}"),
            LiteralOrReference::VirtualReference(reg) => return Err(Error::VirtualRegister(*reg)),
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_virtual_registers_without_writing_partial_assembly() {
        let instructions = Instructions::new(vec![
            il::InstructionNode::new(Instruction::Yield, None),
            il::InstructionNode::new(
                Instruction::Move(Operand::Register(1), Operand::VirtualRegister(7)),
                None,
            ),
        ]);
        let mut writer = BufWriter::new(Vec::new());

        assert!(matches!(
            write(instructions, &mut writer),
            Err(Error::VirtualRegister(7))
        ));
        assert!(writer.into_inner().unwrap().is_empty());
    }

    #[test]
    fn emits_ic10_text_with_existing_spacing_and_newline() {
        let instructions = Instructions::new(vec![
            il::InstructionNode::new(
                Instruction::Move(Operand::Register(1), Operand::Number(5.into())),
                None,
            ),
            il::InstructionNode::new(Instruction::Yield, None),
        ]);
        let mut writer = BufWriter::new(Vec::new());

        write(instructions, &mut writer).unwrap();

        assert_eq!(writer.into_inner().unwrap(), b"move r1 5\nyield\n");
    }

    #[test]
    fn rejects_virtual_registers_nested_in_device_references() {
        let instructions = Instructions::new(vec![il::InstructionNode::new(
            Instruction::Load(
                Operand::Register(1),
                Operand::DeviceReference(DeviceReference::Pin(
                    LiteralOrReference::VirtualReference(11),
                )),
                Operand::LogicType("On".into()),
            ),
            None,
        )]);
        let mut writer = BufWriter::new(Vec::new());

        assert!(matches!(
            write(instructions, &mut writer),
            Err(Error::VirtualRegister(11))
        ));
        assert!(writer.into_inner().unwrap().is_empty());
    }
}
