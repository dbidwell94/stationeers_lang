use helpers::Span;
use parser::tree_node::DeviceType;
use rust_decimal::Decimal;
use std::borrow::Cow;
use std::collections::HashMap;
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

#[derive(Clone)]
pub struct InstructionNode<'a> {
    pub instruction: Instruction<'a>,
    pub span: Option<Span>,
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
    /// A compiler-managed register that must be assigned a hardware register
    /// before the instructions are emitted as IC10.
    VirtualRegister(u32),
    /// A device alias or direct connection (d0-d5, db, $ref)
    Device(DeviceType),
    /// A device reference (e.g., $ref). This is used when we need to strip
    /// the device letter. Example: d0 would be 0. We can then store 0
    /// in a register and use it like: `dr0` for device (register 0)
    DeviceReference(DeviceReference),
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

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralOrReference {
    /// This represents a device reference that is not in a register,
    /// this is a constant and should be treated as such until attempting
    /// to use it in an instruction.
    Literal(Decimal),
    /// This represents a device reference that is stored in a register.
    Reference(u8),
    /// This represents a device reference stored in a virtual register.
    VirtualReference(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeviceReference {
    Housing(LiteralOrReference),
    /// Represents a device pin (e.g., d0, d1) that is stored in a register.
    /// This would resolve to `dr<number>` where <number> is the register
    /// the pin number itself is stored.
    Pin(LiteralOrReference),
    /// Represents a device reference (e.g., $ref) that is stored in a register.
    /// This would resolve to `r<number>` where number is the register the refId
    /// is stored.
    Reference(LiteralOrReference),
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

    /// `lbs register deviceHash slotIndex logicSlotType batchMode` - Load Batch Slot
    LoadBatchSlot(
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
    ),
    /// `lbns register deviceHash nameHash slotIndex logicSlotType batchMode` - Load Batch Named Slot
    LoadBatchNamedSlot(
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
        Operand<'a>,
    ),
    /// `sdse register device ` - Device is set
    DeviceSet(Operand<'a>, Operand<'a>),
    /// `sdns register device ` - Device is not set
    DeviceNotSet(Operand<'a>, Operand<'a>),

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

impl<'a> Instruction<'a> {
    /// Visits every operand in source order, including destinations and control-flow targets.
    pub fn visit_operands(&self, mut visitor: impl FnMut(&Operand<'a>)) {
        macro_rules! visit {
            ($($operand:expr),+ $(,)?) => {{
                $(visitor($operand);)+
            }};
        }

        match self {
            Instruction::Move(a, b)
            | Instruction::Acos(a, b)
            | Instruction::Asin(a, b)
            | Instruction::Atan(a, b)
            | Instruction::Abs(a, b)
            | Instruction::Ceil(a, b)
            | Instruction::Cos(a, b)
            | Instruction::Floor(a, b)
            | Instruction::Log(a, b)
            | Instruction::Sin(a, b)
            | Instruction::Sqrt(a, b)
            | Instruction::Tan(a, b)
            | Instruction::Trunc(a, b)
            | Instruction::DeviceSet(a, b)
            | Instruction::DeviceNotSet(a, b)
            | Instruction::Not(a, b)
            | Instruction::BranchEqZero(a, b)
            | Instruction::BranchNeZero(a, b) => visit!(a, b),
            Instruction::Add(a, b, c)
            | Instruction::Sub(a, b, c)
            | Instruction::Mul(a, b, c)
            | Instruction::Div(a, b, c)
            | Instruction::Mod(a, b, c)
            | Instruction::Pow(a, b, c)
            | Instruction::Atan2(a, b, c)
            | Instruction::Max(a, b, c)
            | Instruction::Min(a, b, c)
            | Instruction::Load(a, b, c)
            | Instruction::Store(a, b, c)
            | Instruction::StoreBatch(a, b, c)
            | Instruction::Rmap(a, b, c)
            | Instruction::BranchEq(a, b, c)
            | Instruction::BranchNe(a, b, c)
            | Instruction::BranchGt(a, b, c)
            | Instruction::BranchLt(a, b, c)
            | Instruction::BranchGe(a, b, c)
            | Instruction::BranchLe(a, b, c)
            | Instruction::SetEq(a, b, c)
            | Instruction::SetNe(a, b, c)
            | Instruction::SetGt(a, b, c)
            | Instruction::SetLt(a, b, c)
            | Instruction::SetGe(a, b, c)
            | Instruction::SetLe(a, b, c)
            | Instruction::And(a, b, c)
            | Instruction::Or(a, b, c)
            | Instruction::Xor(a, b, c)
            | Instruction::Nor(a, b, c)
            | Instruction::Sll(a, b, c)
            | Instruction::Sra(a, b, c)
            | Instruction::Srl(a, b, c)
            | Instruction::Get(a, b, c)
            | Instruction::Put(a, b, c) => visit!(a, b, c),
            Instruction::LoadSlot(a, b, c, d)
            | Instruction::StoreSlot(a, b, c, d)
            | Instruction::LoadBatch(a, b, c, d)
            | Instruction::StoreBatchNamed(a, b, c, d)
            | Instruction::LoadReagent(a, b, c, d)
            | Instruction::Select(a, b, c, d) => visit!(a, b, c, d),
            Instruction::LoadBatchNamed(a, b, c, d, e)
            | Instruction::LoadBatchSlot(a, b, c, d, e) => visit!(a, b, c, d, e),
            Instruction::LoadBatchNamedSlot(a, b, c, d, e, f) => visit!(a, b, c, d, e, f),
            Instruction::Rand(a)
            | Instruction::Jump(a)
            | Instruction::JumpAndLink(a)
            | Instruction::JumpRelative(a)
            | Instruction::Push(a)
            | Instruction::Pop(a)
            | Instruction::Peek(a)
            | Instruction::Sleep(a)
            | Instruction::Clr(a) => visit!(a),
            Instruction::Alias(_, target) => visit!(target),
            Instruction::Yield
            | Instruction::Define(_, _)
            | Instruction::LabelDef(_)
            | Instruction::Comment(_) => {}
        }
    }

    /// Mutably visits every operand in source order, including destinations and control-flow targets.
    pub fn visit_operands_mut(&mut self, mut visitor: impl FnMut(&mut Operand<'a>)) {
        macro_rules! visit {
            ($($operand:expr),+ $(,)?) => {{
                $(visitor($operand);)+
            }};
        }

        match self {
            Instruction::Move(a, b)
            | Instruction::Acos(a, b)
            | Instruction::Asin(a, b)
            | Instruction::Atan(a, b)
            | Instruction::Abs(a, b)
            | Instruction::Ceil(a, b)
            | Instruction::Cos(a, b)
            | Instruction::Floor(a, b)
            | Instruction::Log(a, b)
            | Instruction::Sin(a, b)
            | Instruction::Sqrt(a, b)
            | Instruction::Tan(a, b)
            | Instruction::Trunc(a, b)
            | Instruction::DeviceSet(a, b)
            | Instruction::DeviceNotSet(a, b)
            | Instruction::Not(a, b)
            | Instruction::BranchEqZero(a, b)
            | Instruction::BranchNeZero(a, b) => visit!(a, b),
            Instruction::Add(a, b, c)
            | Instruction::Sub(a, b, c)
            | Instruction::Mul(a, b, c)
            | Instruction::Div(a, b, c)
            | Instruction::Mod(a, b, c)
            | Instruction::Pow(a, b, c)
            | Instruction::Atan2(a, b, c)
            | Instruction::Max(a, b, c)
            | Instruction::Min(a, b, c)
            | Instruction::Load(a, b, c)
            | Instruction::Store(a, b, c)
            | Instruction::StoreBatch(a, b, c)
            | Instruction::Rmap(a, b, c)
            | Instruction::BranchEq(a, b, c)
            | Instruction::BranchNe(a, b, c)
            | Instruction::BranchGt(a, b, c)
            | Instruction::BranchLt(a, b, c)
            | Instruction::BranchGe(a, b, c)
            | Instruction::BranchLe(a, b, c)
            | Instruction::SetEq(a, b, c)
            | Instruction::SetNe(a, b, c)
            | Instruction::SetGt(a, b, c)
            | Instruction::SetLt(a, b, c)
            | Instruction::SetGe(a, b, c)
            | Instruction::SetLe(a, b, c)
            | Instruction::And(a, b, c)
            | Instruction::Or(a, b, c)
            | Instruction::Xor(a, b, c)
            | Instruction::Nor(a, b, c)
            | Instruction::Sll(a, b, c)
            | Instruction::Sra(a, b, c)
            | Instruction::Srl(a, b, c)
            | Instruction::Get(a, b, c)
            | Instruction::Put(a, b, c) => visit!(a, b, c),
            Instruction::LoadSlot(a, b, c, d)
            | Instruction::StoreSlot(a, b, c, d)
            | Instruction::LoadBatch(a, b, c, d)
            | Instruction::StoreBatchNamed(a, b, c, d)
            | Instruction::LoadReagent(a, b, c, d)
            | Instruction::Select(a, b, c, d) => visit!(a, b, c, d),
            Instruction::LoadBatchNamed(a, b, c, d, e)
            | Instruction::LoadBatchSlot(a, b, c, d, e) => visit!(a, b, c, d, e),
            Instruction::LoadBatchNamedSlot(a, b, c, d, e, f) => visit!(a, b, c, d, e, f),
            Instruction::Rand(a)
            | Instruction::Jump(a)
            | Instruction::JumpAndLink(a)
            | Instruction::JumpRelative(a)
            | Instruction::Push(a)
            | Instruction::Pop(a)
            | Instruction::Peek(a)
            | Instruction::Sleep(a)
            | Instruction::Clr(a) => visit!(a),
            Instruction::Alias(_, target) => visit!(target),
            Instruction::Yield
            | Instruction::Define(_, _)
            | Instruction::LabelDef(_)
            | Instruction::Comment(_) => {}
        }
    }
}
