use helpers::Span;
use rust_decimal::Decimal;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::io::{BufWriter, Write};
use std::ops::{Deref, DerefMut};

#[derive(Default)]
pub struct Instructions<'a>(Vec<InstructionNode<'a>>);

impl<'a> Deref for Instructions<'a> {
    type Target = Vec<InstructionNode<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Instructions<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> Instructions<'a> {
    pub fn new(instructions: Vec<InstructionNode<'a>>) -> Self {
        Self(instructions)
    }
    pub fn into_inner(self) -> Vec<InstructionNode<'a>> {
        self.0
    }
    pub fn write<W: Write>(self, writer: &mut BufWriter<W>) -> Result<(), std::io::Error> {
        for node in self.0 {
            writer.write_all(node.to_string().as_bytes())?;
            writer.write_all(b"\n")?;
        }

        writer.flush()?;
        Ok(())
    }
    pub fn source_map(&self) -> HashMap<usize, Span> {
        let mut map = HashMap::new();

        for (line_num, node) in self.0.iter().enumerate() {
            if let Some(span) = node.span {
                map.insert(line_num, span);
            }
        }

        map
    }
}

impl<'a> std::fmt::Display for Instructions<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in &self.0 {
            writeln!(f, "{node}")?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct InstructionNode<'a> {
    pub instruction: Instruction<'a>,
    pub span: Option<Span>,
}

impl<'a> std::fmt::Display for InstructionNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.instruction)
    }
}

impl<'a> InstructionNode<'a> {
    pub fn new(instr: Instruction<'a>, span: Option<Span>) -> Self {
        Self {
            span,
            instruction: instr,
        }
    }
}

/// Represents the different types of operands available in IC10.
#[derive(Debug, Clone, PartialEq)]
pub enum Operand<'a> {
    /// A hardware register (r0-r15)
    Register(u8),
    /// A device alias or direct connection (d0-d5, db)
    Device(Cow<'a, str>),
    /// A numeric literal (integer or float)
    Number(Decimal),
    /// A label used for jumping
    Label(Cow<'a, str>),
    /// A logic type string (e.g., "Temperature", "Open")
    LogicType(Cow<'a, str>),
    /// Special register: Stack Pointer
    StackPointer,
    /// Special register: Return Address
    ReturnAddress,
}

impl<'a> fmt::Display for Operand<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Register(r) => write!(f, "r{}", r),
            Operand::Device(d) => write!(f, "{}", d),
            Operand::Number(n) => write!(f, "{}", n),
            Operand::Label(l) => write!(f, "{}", l),
            Operand::LogicType(t) => write!(f, "{}", t),
            Operand::StackPointer => write!(f, "sp"),
            Operand::ReturnAddress => write!(f, "ra"),
        }
    }
}

/// Represents a single IC10 MIPS instruction.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction<'a> {
    /// `move dst val` - Copy value to register
    Move(Operand<'a>, Operand<'a>),

    /// `add dst a b` - Addition
    Add(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `sub dst a b` - Subtraction
    Sub(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `mul dst a b` - Multiplication
    Mul(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `div dst a b` - Division
    Div(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `mod dst a b` - Modulo
    Mod(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `pow dst a b` - Power
    Pow(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `acos dst a`
    Acos(Operand<'a>, Operand<'a>),
    /// `asin dst a`
    Asin(Operand<'a>, Operand<'a>),
    /// `atan dst a`
    Atan(Operand<'a>, Operand<'a>),
    /// `atan2 dst a b`
    Atan2(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `abs dst a`
    Abs(Operand<'a>, Operand<'a>),
    /// `ceil dst a`
    Ceil(Operand<'a>, Operand<'a>),
    /// `cos dst a`
    Cos(Operand<'a>, Operand<'a>),
    /// `floor dst a`
    Floor(Operand<'a>, Operand<'a>),
    /// `log dst a`
    Log(Operand<'a>, Operand<'a>),
    /// `max dst a b`
    Max(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `min dst a b`
    Min(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `rand dst`
    Rand(Operand<'a>),
    /// `sin dst a`
    Sin(Operand<'a>, Operand<'a>),
    /// `sqrt dst a`
    Sqrt(Operand<'a>, Operand<'a>),
    /// `tan dst a`
    Tan(Operand<'a>, Operand<'a>),
    /// `trunc dst a`
    Trunc(Operand<'a>, Operand<'a>),

    /// `l register device type` - Load from device
    Load(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `s device type value` - Set on device
    Store(Operand<'a>, Operand<'a>, Operand<'a>),

    /// `ls register device slot type` - Load Slot
    LoadSlot(Operand<'a>, Operand<'a>, Operand<'a>, Operand<'a>),
    /// `ss device slot type value` - Set Slot
    StoreSlot(Operand<'a>, Operand<'a>, Operand<'a>, Operand<'a>),

    /// `lb register deviceHash type batchMode` - Load Batch
    LoadBatch(Operand<'a>, Operand<'a>, Operand<'a>, Operand<'a>),
    /// `sb deviceHash type value` - Set Batch
    StoreBatch(Operand<'a>, Operand<'a>, Operand<'a>),

    /// `lbn register deviceHash nameHash type batchMode` - Load Batch Named
    LoadBatchNamed(
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
    ),
    /// `sbn deviceHash nameHash type value` - Set Batch Named
    StoreBatchNamed(Operand<'a>, Operand<'a>, Operand<'a>, Operand<'a>),

    /// `lr register device reagentMode int`
    LoadReagent(Operand<'a>, Operand<'a>, Operand<'a>, Operand<'a>),

    /// `rmap register device reagentHash` - Resolve Reagent to Item Hash
    Rmap(Operand<'a>, Operand<'a>, Operand<'a>),

    /// `j label` - Unconditional Jump
    Jump(Operand<'a>),
    /// `jal label` - Jump and Link (Function Call)
    JumpAndLink(Operand<'a>),
    /// `jr offset` - Jump Relative
    JumpRelative(Operand<'a>),

    /// `beq a b label` - Branch if Equal
    BranchEq(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `bne a b label` - Branch if Not Equal
    BranchNe(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `bgt a b label` - Branch if Greater Than
    BranchGt(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `blt a b label` - Branch if Less Than
    BranchLt(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `bge a b label` - Branch if Greater or Equal
    BranchGe(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `ble a b label` - Branch if Less or Equal
    BranchLe(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `beqz a label` - Branch if Equal Zero
    BranchEqZero(Operand<'a>, Operand<'a>),
    /// `bnez a label` - Branch if Not Equal Zero
    BranchNeZero(Operand<'a>, Operand<'a>),

    /// `seq dst a b` - Set if Equal
    SetEq(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `sne dst a b` - Set if Not Equal
    SetNe(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `sgt dst a b` - Set if Greater Than
    SetGt(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `slt dst a b` - Set if Less Than
    SetLt(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `sge dst a b` - Set if Greater or Equal
    SetGe(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `sle dst a b` - Set if Less or Equal
    SetLe(Operand<'a>, Operand<'a>, Operand<'a>),

    /// `and dst a b` - Bitwise AND
    And(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `or dst a b` - Bitwise OR
    Or(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `xor dst a b` - Bitwise XOR
    Xor(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `nor dst a b` - Bitwise NOR
    Nor(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `not dst a` - Bitwise NOT
    Not(Operand<'a>, Operand<'a>),
    /// `sll dst a b` - Logical Left Shift
    Sll(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `sra dst a b` - Arithmetic Right Shift
    Sra(Operand<'a>, Operand<'a>, Operand<'a>),
    /// `srl dst a b` - Logical Right Shift
    Srl(Operand<'a>, Operand<'a>, Operand<'a>),

    /// `push val` - Push to Stack
    Push(Operand<'a>),
    /// `pop dst` - Pop from Stack
    Pop(Operand<'a>),
    /// `peek dst` - Peek from Stack (Usually sp - 1)
    Peek(Operand<'a>),
    /// `get dst dev num`
    Get(Operand<'a>, Operand<'a>, Operand<'a>),
    /// put dev addr val
    Put(Operand<'a>, Operand<'a>, Operand<'a>),

    /// `select dst cond a b` - Ternary Select
    Select(Operand<'a>, Operand<'a>, Operand<'a>, Operand<'a>),

    /// `yield` - Pause execution
    Yield,
    /// `sleep val` - Sleep for seconds
    Sleep(Operand<'a>),
    /// `clr val` - Clear stack memory on device
    Clr(Operand<'a>),

    /// `alias name target` - Define Alias (Usually handled by compiler, but good for IR)
    Alias(Cow<'a, str>, Operand<'a>),
    /// `define name val` - Define Constant (Usually handled by compiler)
    Define(Cow<'a, str>, f64),

    /// A label definition `Label:`
    LabelDef(Cow<'a, str>),

    /// A comment `# text`
    Comment(Cow<'a, str>),
}

impl<'a> fmt::Display for Instruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Move(dst, val) => write!(f, "move {} {}", dst, val),
            Instruction::Add(dst, a, b) => write!(f, "add {} {} {}", dst, a, b),
            Instruction::Sub(dst, a, b) => write!(f, "sub {} {} {}", dst, a, b),
            Instruction::Mul(dst, a, b) => write!(f, "mul {} {} {}", dst, a, b),
            Instruction::Div(dst, a, b) => write!(f, "div {} {} {}", dst, a, b),
            Instruction::Mod(dst, a, b) => write!(f, "mod {} {} {}", dst, a, b),
            Instruction::Pow(dst, a, b) => write!(f, "pow {} {} {}", dst, a, b),
            Instruction::Acos(dst, a) => write!(f, "acos {} {}", dst, a),
            Instruction::Asin(dst, a) => write!(f, "asin {} {}", dst, a),
            Instruction::Atan(dst, a) => write!(f, "atan {} {}", dst, a),
            Instruction::Atan2(dst, a, b) => write!(f, "atan2 {} {} {}", dst, a, b),
            Instruction::Abs(dst, a) => write!(f, "abs {} {}", dst, a),
            Instruction::Ceil(dst, a) => write!(f, "ceil {} {}", dst, a),
            Instruction::Cos(dst, a) => write!(f, "cos {} {}", dst, a),
            Instruction::Floor(dst, a) => write!(f, "floor {} {}", dst, a),
            Instruction::Log(dst, a) => write!(f, "log {} {}", dst, a),
            Instruction::Max(dst, a, b) => write!(f, "max {} {} {}", dst, a, b),
            Instruction::Min(dst, a, b) => write!(f, "min {} {} {}", dst, a, b),
            Instruction::Rand(dst) => write!(f, "rand {}", dst),
            Instruction::Sin(dst, a) => write!(f, "sin {} {}", dst, a),
            Instruction::Sqrt(dst, a) => write!(f, "sqrt {} {}", dst, a),
            Instruction::Tan(dst, a) => write!(f, "tan {} {}", dst, a),
            Instruction::Trunc(dst, a) => write!(f, "trunc {} {}", dst, a),

            Instruction::Load(reg, dev, typ) => write!(f, "l {} {} {}", reg, dev, typ),
            Instruction::Store(dev, typ, val) => write!(f, "s {} {} {}", dev, typ, val),
            Instruction::LoadSlot(reg, dev, slot, typ) => {
                write!(f, "ls {} {} {} {}", reg, dev, slot, typ)
            }
            Instruction::StoreSlot(dev, slot, typ, val) => {
                write!(f, "ss {} {} {} {}", dev, slot, typ, val)
            }
            Instruction::LoadBatch(reg, hash, typ, mode) => {
                write!(f, "lb {} {} {} {}", reg, hash, typ, mode)
            }
            Instruction::StoreBatch(hash, typ, val) => write!(f, "sb {} {} {}", hash, typ, val),
            Instruction::LoadBatchNamed(reg, d_hash, n_hash, typ, mode) => {
                write!(f, "lbn {} {} {} {} {}", reg, d_hash, n_hash, typ, mode)
            }
            Instruction::StoreBatchNamed(d_hash, n_hash, typ, val) => {
                write!(f, "sbn {} {} {} {}", d_hash, n_hash, typ, val)
            }
            Instruction::LoadReagent(reg, device, reagent_mode, reagent_hash) => {
                write!(f, "lr {} {} {} {}", reg, device, reagent_mode, reagent_hash)
            }
            Instruction::Rmap(reg, device, reagent_hash) => {
                write!(f, "rmap {} {} {}", reg, device, reagent_hash)
            }
            Instruction::Jump(lbl) => write!(f, "j {}", lbl),
            Instruction::JumpAndLink(lbl) => write!(f, "jal {}", lbl),
            Instruction::JumpRelative(off) => write!(f, "jr {}", off),
            Instruction::BranchEq(a, b, lbl) => write!(f, "beq {} {} {}", a, b, lbl),
            Instruction::BranchNe(a, b, lbl) => write!(f, "bne {} {} {}", a, b, lbl),
            Instruction::BranchGt(a, b, lbl) => write!(f, "bgt {} {} {}", a, b, lbl),
            Instruction::BranchLt(a, b, lbl) => write!(f, "blt {} {} {}", a, b, lbl),
            Instruction::BranchGe(a, b, lbl) => write!(f, "bge {} {} {}", a, b, lbl),
            Instruction::BranchLe(a, b, lbl) => write!(f, "ble {} {} {}", a, b, lbl),
            Instruction::BranchEqZero(a, lbl) => write!(f, "beqz {} {}", a, lbl),
            Instruction::BranchNeZero(a, lbl) => write!(f, "bnez {} {}", a, lbl),
            Instruction::SetEq(dst, a, b) => write!(f, "seq {} {} {}", dst, a, b),
            Instruction::SetNe(dst, a, b) => write!(f, "sne {} {} {}", dst, a, b),
            Instruction::SetGt(dst, a, b) => write!(f, "sgt {} {} {}", dst, a, b),
            Instruction::SetLt(dst, a, b) => write!(f, "slt {} {} {}", dst, a, b),
            Instruction::SetGe(dst, a, b) => write!(f, "sge {} {} {}", dst, a, b),
            Instruction::SetLe(dst, a, b) => write!(f, "sle {} {} {}", dst, a, b),
            Instruction::And(dst, a, b) => write!(f, "and {} {} {}", dst, a, b),
            Instruction::Or(dst, a, b) => write!(f, "or {} {} {}", dst, a, b),
            Instruction::Xor(dst, a, b) => write!(f, "xor {} {} {}", dst, a, b),
            Instruction::Nor(dst, a, b) => write!(f, "nor {} {} {}", dst, a, b),
            Instruction::Not(dst, a) => write!(f, "not {} {}", dst, a),
            Instruction::Sll(dst, a, b) => write!(f, "sll {} {} {}", dst, a, b),
            Instruction::Sra(dst, a, b) => write!(f, "sra {} {} {}", dst, a, b),
            Instruction::Srl(dst, a, b) => write!(f, "srl {} {} {}", dst, a, b),
            Instruction::Push(val) => write!(f, "push {}", val),
            Instruction::Pop(dst) => write!(f, "pop {}", dst),
            Instruction::Peek(dst) => write!(f, "peek {}", dst),
            Instruction::Get(dst, dev, val) => write!(f, "get {} {} {}", dst, dev, val),
            Instruction::Put(dev, addr, val) => write!(f, "put {} {} {}", dev, addr, val),
            Instruction::Select(dst, cond, a, b) => {
                write!(f, "select {} {} {} {}", dst, cond, a, b)
            }
            Instruction::Yield => write!(f, "yield"),
            Instruction::Sleep(val) => write!(f, "sleep {}", val),
            Instruction::Clr(val) => write!(f, "clr {}", val),
            Instruction::Alias(name, target) => write!(f, "alias {} {}", name, target),
            Instruction::Define(name, val) => write!(f, "define {} {}", name, val),
            Instruction::LabelDef(lbl) => write!(f, "{}:", lbl),
            Instruction::Comment(c) => write!(f, "# {}", c),
        }
    }
}
