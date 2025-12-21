use super::LiteralOrVariable;
use crate::tree_node::{Expression, Literal, Spanned};
use helpers::prelude::*;

documented! {
    #[derive(Debug, PartialEq, Eq)]
    pub enum Math<'a> {
        /// Returns the angle in radians whose cosine is the specified number.
        /// ## IC10
        /// `acos r? a(r?|num)`
        /// ## Slang
        /// `let item = acos(number|var|expression);`
        Acos(Box<Spanned<Expression<'a>>>),
        /// Returns the angle in radians whose sine is the specified number.
        /// ## IC10
        /// `asin r? a(r?|num)`
        /// ## Slang
        /// `let item = asin(number|var|expression);`
        Asin(Box<Spanned<Expression<'a>>>),
        /// Returns the angle in radians whose tangent is the specified number.
        /// ## IC10
        /// `atan r? a(r?|num)`
        /// ## Slang
        /// `let item = atan(number|var|expression);`
        Atan(Box<Spanned<Expression<'a>>>),
        /// Returns the angle in radians whose tangent is the quotient of the specified numbers.
        /// ## IC10
        /// `atan2 r? a(r?|num) b(r?|num)`
        /// ## Slang
        /// `let item = atan2((number|var|expression), (number|var|expression));`
        Atan2(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
        /// Gets the absolute value of a number.
        /// ## IC10
        /// `abs r? a(r?|num)`
        /// ## Slang
        /// `let item = abs((number|var|expression));`
        Abs(Box<Spanned<Expression<'a>>>),
        /// Rounds a number up to the nearest whole number.
        /// ## IC10
        /// `ceil r? a(r?|num)`
        /// ## Slang
        /// `let item = ceil((number|var|expression));`
        Ceil(Box<Spanned<Expression<'a>>>),
        /// Returns the cosine of the specified angle in radians.
        /// ## IC10
        /// `cos r? a(r?|num)`
        /// ## Slang
        /// `let item = cos((number|var|expression));`
        Cos(Box<Spanned<Expression<'a>>>),
        /// Rounds a number down to the nearest whole number.
        /// ## IC10
        /// `floor r? a(r?|num)`
        /// ## Slang
        /// `let item = floor((number|var|expression));`
        Floor(Box<Spanned<Expression<'a>>>),
        /// Computes the natural logarithm of a number.
        /// ## IC10
        /// `log r? a(r?|num)`
        /// ## Slang
        /// `let item = log((number|var|expression));`
        Log(Box<Spanned<Expression<'a>>>),
        /// Computes the maximum of two numbers.
        /// ## IC10
        /// `max r? a(r?|num) b(r?|num)`
        /// ## Slang
        /// `let item = max((number|var|expression), (number|var|expression));`
        Max(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
        /// Computes the minimum of two numbers.
        /// ## IC10
        /// `min r? a(r?|num) b(r?|num)`
        /// ## Slang
        /// `let item = min((number|var|expression), (number|var|expression));`
        Min(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
        /// Gets a random number between 0 and 1.
        /// ## IC10
        /// `rand r?`
        /// ## Slang
        /// `let item = rand();`
        Rand,
        /// Returns the sine of the specified angle in radians.
        /// ## IC10
        /// `sin r? a(r?|num)`
        /// ## Slang
        /// `let item = sin((number|var|expression));`
        Sin(Box<Spanned<Expression<'a>>>),
        /// Computes the square root of a number.
        /// ## IC10
        /// `sqrt r? a(r?|num)`
        /// ## Slang
        /// `let item = sqrt((number|var|expression));`
        Sqrt(Box<Spanned<Expression<'a>>>),
        /// Returns the tangent of the specified angle in radians.
        /// ## IC10
        /// `tan r? a(r?|num)`
        /// ## Slang
        /// `let item = tan((number|var|expression));`
        Tan(Box<Spanned<Expression<'a>>>),
        /// Truncates a number by removing the decimal portion.
        /// ## IC10
        /// `trunc r? a(r?|num)`
        /// ## Slang
        /// `let item = trunc((number|var|expression));`
        Trunc(Box<Spanned<Expression<'a>>>),
    }
}

impl<'a> std::fmt::Display for Math<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Math::Acos(a) => write!(f, "acos({})", a),
            Math::Asin(a) => write!(f, "asin({})", a),
            Math::Atan(a) => write!(f, "atan({})", a),
            Math::Atan2(a, b) => write!(f, "atan2({}, {})", a, b),
            Math::Abs(a) => write!(f, "abs({})", a),
            Math::Ceil(a) => write!(f, "ceil({})", a),
            Math::Cos(a) => write!(f, "cos({})", a),
            Math::Floor(a) => write!(f, "floor({})", a),
            Math::Log(a) => write!(f, "log({})", a),
            Math::Max(a, b) => write!(f, "max({}, {})", a, b),
            Math::Min(a, b) => write!(f, "min({}, {})", a, b),
            Math::Rand => write!(f, "rand()"),
            Math::Sin(a) => write!(f, "sin({})", a),
            Math::Sqrt(a) => write!(f, "sqrt({})", a),
            Math::Tan(a) => write!(f, "tan({})", a),
            Math::Trunc(a) => write!(f, "trunc({})", a),
        }
    }
}

documented! {
    #[derive(Debug, PartialEq, Eq)]
    pub enum System<'a> {
        /// Pauses execution for exactly 1 tick and then resumes.
        /// ## IC10
        /// `yield`
        /// ## Slang
        /// `yield();`
        Yield,
        /// Represents a function that can be called to sleep for a certain amount of time.
        /// ## IC10
        /// `sleep a(r?|num)`
        /// ## Slang
        /// `sleep(number|var);`
        Sleep(Box<Spanned<Expression<'a>>>),
        /// Gets the in-game hash for a specific prefab name. NOTE! This call is COMPLETELY
        /// optimized away unless you bind it to a `let` variable. If you use a `const` variable
        /// however, the hash is correctly computed at compile time and substitued automatically.
        /// ## IC10
        /// `HASH("prefabName")`
        /// ## Slang
        /// `hash("prefabName");`
        ///
        /// ## Example
        /// ```
        /// const compDoor = hash("StructureCompositeDoor");
        /// setOnDeviceBatched(compDoor, "Lock", true);
        /// ```
        Hash(Spanned<Literal<'a>>),
        /// Represents a function which loads a device variable into a register.
        /// ## IC10
        /// `l r? d? var`
        /// ## Slang
        /// `let item = load(deviceHash, "LogicType");`
        /// `let item = l(deviceHash, "LogicType");`
        /// `let item = deviceAlias.LogicType;`
        LoadFromDevice(Spanned<LiteralOrVariable<'a>>, Spanned<Literal<'a>>),
        /// Function which gets a LogicType from all connected network devices that match
        /// the provided device hash and name, aggregating them via a batchMode
        /// ## IC10
        /// `lbn r? deviceHash nameHash logicType batchMode`
        /// ## Slang
        /// `loadBatchedNamed(deviceHash, deviceName, "LogicType", "BatchMode");`
        /// `lbn(deviceHash, deviceName, "LogicType", "BatchMode");`
        LoadBatchNamed(
            Spanned<LiteralOrVariable<'a>>,
            Spanned<LiteralOrVariable<'a>>,
            Spanned<Literal<'a>>,
            Spanned<Literal<'a>>,
        ),
        /// Loads a LogicType from all connected network devices, aggregating them via a
        /// BatchMode
        /// ## IC10
        /// `lb r? deviceHash logicType batchMode`
        /// ## Slang
        /// `loadBatched(deviceHash, "Variable", "LogicType");`
        /// `lb(deviceHash, "Variable", "LogicType");`
        LoadBatch(Spanned<LiteralOrVariable<'a>>, Spanned<Literal<'a>>, Spanned<Literal<'a>>),
        /// Represents a function which stores a setting into a specific device.
        /// ## IC10
        /// `s d? logicType r?`
        /// ## Slang
        /// `set(deviceHash, "LogicType", (number|var));`
        /// `s(deviceHash, "LogicType", (number|var));`
        /// `deviceAlias.LogicType = (number|var);`
        SetOnDevice(Spanned<LiteralOrVariable<'a>>, Spanned<Literal<'a>>, Box<Spanned<Expression<'a>>>),
        /// Represents a function which stores a setting to all devices that match
        /// the given deviceHash
        /// ## IC10
        /// `sb deviceHash logicType r?`
        /// ## Slang
        /// `setBatched(deviceHash, "LogicType", (number|var));`
        /// `sb(deviceHash, "LogicType", (number|var));`
        SetOnDeviceBatched(Spanned<LiteralOrVariable<'a>>, Spanned<Literal<'a>>, Box<Spanned<Expression<'a>>>),
        /// Represents a function which stores a setting to all devices that match
        /// both the given deviceHash AND the given nameHash
        /// ## IC10
        /// `sbn deviceHash nameHash logicType r?`
        /// ## Slang
        /// `setBatchedNamed(deviceHash, nameHash, "LogicType", (number|var));`
        /// `sbn(deviceHash, nameHash, "LogicType", (number|var));`
        SetOnDeviceBatchedNamed(
            Spanned<LiteralOrVariable<'a>>,
            Spanned<LiteralOrVariable<'a>>,
            Spanned<Literal<'a>>,
            Box<Spanned<Expression<'a>>>,
        ),
        /// Loads slot LogicSlotType from device into a variable
        ///
        /// ## IC10
        /// `ls r0 d0 2 Occupied`
        /// ## Slang
        /// `let isOccupied = loadSlot(deviceHash, 2, "Occupied");`
        /// `let isOccupied = ls(deviceHash, 2, "Occupied");`
        LoadSlot(
            Spanned<LiteralOrVariable<'a>>,
            Box<Spanned<Expression<'a>>>,
            Spanned<Literal<'a>>
        ),
        /// Stores a value of LogicType on a device by the index value
        /// ## IC10
        /// `ss d0 0 "Open" 1`
        /// ## Slang
        /// `setSlot(deviceHash, 0, "Open", true);`
        /// `ss(deviceHash, 0, "Open", true);`
        SetSlot(
            Spanned<LiteralOrVariable<'a>>,
            Box<Spanned<Expression<'a>>>,
            Spanned<Literal<'a>>,
            Box<Spanned<Expression<'a>>>
        ),
        /// Loads reagent of device's ReagentMode where a hash of the reagent type to check for
        ///
        /// ## IC10
        /// `lr r? device(d?|r?|id) reagentMode int`
        /// ## Slang
        /// `let result = loadReagent(deviceHash, "ReagentMode", reagentHash);`
        /// `let result = lr(deviceHash, "ReagentMode", reagentHash);`
        LoadReagent(
            Spanned<LiteralOrVariable<'a>>,
            Spanned<Literal<'a>>,
            Box<Spanned<Expression<'a>>>
        )
    }
}

impl<'a> std::fmt::Display for System<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            System::Yield => write!(f, "yield()"),
            System::Sleep(a) => write!(f, "sleep({})", a),
            System::Hash(a) => write!(f, "hash({})", a),
            System::LoadFromDevice(a, b) => write!(f, "loadFromDevice({}, {})", a, b),
            System::LoadBatch(a, b, c) => write!(f, "loadBatch({}, {}, {})", a, b, c),
            System::LoadBatchNamed(a, b, c, d) => {
                write!(f, "loadBatchNamed({}, {}, {}, {})", a, b, c, d)
            }
            System::SetOnDevice(a, b, c) => write!(f, "setOnDevice({}, {}, {})", a, b, c),
            System::SetOnDeviceBatched(a, b, c) => {
                write!(f, "setOnDeviceBatched({}, {}, {})", a, b, c)
            }
            System::SetOnDeviceBatchedNamed(a, b, c, d) => {
                write!(f, "setOnDeviceBatchedNamed({}, {}, {}, {})", a, b, c, d)
            }
            System::LoadSlot(a, b, c) => write!(f, "loadSlot({}, {}, {})", a, b, c),
            System::SetSlot(a, b, c, d) => write!(f, "setSlot({}, {}, {}, {})", a, b, c, d),
            System::LoadReagent(a, b, c) => write!(f, "loadReagent({}, {}, {})", a, b, c),
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Eq)]
/// This represents built in functions that cannot be overwritten, but can be invoked by the user as functions.
pub enum SysCall<'a> {
    System(System<'a>),
    /// Represents any mathmatical function that can be called.
    Math(Math<'a>),
}

impl<'a> Documentation for SysCall<'a> {
    fn docs(&self) -> String {
        match self {
            Self::System(s) => s.docs(),
            Self::Math(m) => m.docs(),
        }
    }

    fn get_all_documentation() -> Vec<(&'static str, String)> {
        let mut all_docs = System::get_all_documentation();
        all_docs.extend(Math::get_all_documentation());

        all_docs
    }
}

impl<'a> std::fmt::Display for SysCall<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SysCall::System(s) => write!(f, "{}", s),
            SysCall::Math(m) => write!(f, "{}", m),
        }
    }
}

impl<'a> SysCall<'a> {
    pub fn is_syscall(identifier: &str) -> bool {
        tokenizer::token::is_syscall(identifier)
    }
}
