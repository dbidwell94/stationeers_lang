use super::LiteralOrVariable;
use crate::tree_node::{Expression, Literal, Spanned};
use helpers::prelude::*;

documented! {
    #[derive(Debug, PartialEq, Eq)]
    pub enum Math {
        /// Returns the angle in radians whose cosine is the specified number.
        /// ## IC10
        /// `acos r? a(r?|num)`
        /// ## Slang
        /// `(number|var).acos();`
        Acos(LiteralOrVariable),
        /// Returns the angle in radians whose sine is the specified number.
        /// ## IC10
        /// `asin r? a(r?|num)`
        /// ## Slang
        /// `(number|var).asin();`
        Asin(LiteralOrVariable),
        /// Returns the angle in radians whose tangent is the specified number.
        /// ## IC10
        /// `atan r? a(r?|num)`
        /// ## Slang
        /// `(number|var).atan();`
        Atan(LiteralOrVariable),
        /// Returns the angle in radians whose tangent is the quotient of the specified numbers.
        /// ## IC10
        /// `atan2 r? a(r?|num) b(r?|num)`
        /// ## Slang
        /// `(number|var).atan2((number|var));`
        Atan2(LiteralOrVariable, LiteralOrVariable),
        /// Gets the absolute value of a number.
        /// ## IC10
        /// `abs r? a(r?|num)`
        /// ## Slang
        /// `(number|var).abs();`
        Abs(LiteralOrVariable),
        /// Rounds a number up to the nearest whole number.
        /// ## IC10
        /// `ceil r? a(r?|num)`
        /// ## Slang
        /// `(number|var).ceil();`
        Ceil(LiteralOrVariable),
        /// Returns the cosine of the specified angle in radians.
        /// ## IC10
        /// `cos r? a(r?|num)`
        /// ## Slang
        /// `(number|var).cos();`
        Cos(LiteralOrVariable),
        /// Rounds a number down to the nearest whole number.
        /// ## IC10
        /// `floor r? a(r?|num)`
        /// ## Slang
        /// `(number|var).floor();`
        Floor(LiteralOrVariable),
        /// Computes the natural logarithm of a number.
        /// ## IC10
        /// `log r? a(r?|num)`
        /// ## Slang
        /// `(number|var).log();`
        Log(LiteralOrVariable),
        /// Computes the maximum of two numbers.
        /// ## IC10
        /// `max r? a(r?|num) b(r?|num)`
        /// ## Slang
        /// `(number|var).max((number|var));`
        Max(LiteralOrVariable, LiteralOrVariable),
        /// Computes the minimum of two numbers.
        /// ## IC10
        /// `min r? a(r?|num) b(r?|num)`
        /// ## Slang
        /// `(number|var).min((number|var));`
        Min(LiteralOrVariable, LiteralOrVariable),
        /// Gets a random number between 0 and 1.
        /// ## IC10
        /// `rand r?`
        /// ## Slang
        /// `rand();`
        Rand,
        /// Returns the sine of the specified angle in radians.
        /// ## IC10
        /// `sin r? a(r?|num)`
        /// ## Slang
        /// `(number|var).sin();`
        Sin(LiteralOrVariable),
        /// Computes the square root of a number.
        /// ## IC10
        /// `sqrt r? a(r?|num)`
        /// ## Slang
        /// `(number|var).sqrt();`
        Sqrt(LiteralOrVariable),
        /// Returns the tangent of the specified angle in radians.
        /// ## IC10
        /// `tan r? a(r?|num)`
        /// ## Slang
        /// `(number|var).tan();`
        Tan(LiteralOrVariable),
        /// Truncates a number by removing the decimal portion.
        /// ## IC10
        /// `trunc r? a(r?|num)`
        /// ## Slang
        /// `(number|var).trunc();`
        Trunc(LiteralOrVariable),
    }
}

impl std::fmt::Display for Math {
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
    pub enum System {
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
        Sleep(Box<Spanned<Expression>>),
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
        Hash(Spanned<Literal>),
        /// Represents a function which loads a device variable into a register.
        /// ## IC10
        /// `l r? d? var`
        /// ## Slang
        /// `load(deviceType, "LogicType");`
        LoadFromDevice(Spanned<LiteralOrVariable>, Spanned<Literal>),
        /// Function which gets a LogicType from all connected network devices that match
        /// the provided device hash and name, aggregating them via a batchMode
        /// ## IC10
        /// `lbn r? deviceHash nameHash logicType batchMode`
        /// ## Slang
        /// `loadBatchedNamed(deviceHash, deviceName, "LogicType", "BatchMode");`
        LoadBatchNamed(
            Spanned<LiteralOrVariable>,
            Spanned<LiteralOrVariable>,
            Spanned<Literal>,
            Spanned<Literal>,
        ),
        /// Loads a LogicType from all connected network devices, aggregating them via a
        /// BatchMode
        /// ## IC10
        /// `lb r? deviceHash logicType batchMode`
        /// ## Slang
        /// `loadBatched(deviceHash, "Variable", "LogicType");`
        LoadBatch(Spanned<LiteralOrVariable>, Spanned<Literal>, Spanned<Literal>),
        /// Represents a function which stores a setting into a specific device.
        /// ## IC10
        /// `s d? logicType r?`
        /// ## Slang
        /// `set(deviceType, "Variable", (number|var));`
        SetOnDevice(Spanned<LiteralOrVariable>, Spanned<Literal>, Box<Spanned<Expression>>),
        /// Represents a function which stores a setting to all devices that match
        /// the given deviceHash
        /// ## IC10
        /// `sb deviceHash logicType r?`
        /// ## Slang
        /// `setBatched(deviceHash, "LogicType", (number|var))`
        SetOnDeviceBatched(Spanned<LiteralOrVariable>, Spanned<Literal>, Box<Spanned<Expression>>),
        /// Represents a function which stores a setting to all devices that match
        /// both the given deviceHash AND the given nameHash
        /// ## IC10
        /// `sbn deviceHash nameHash logicType r?`
        /// ## Slang
        /// `setBatchedNamed(deviceHash, nameHash, "LogicType", (number|var))`
        SetOnDeviceBatchedNamed(
            Spanned<LiteralOrVariable>,
            Spanned<LiteralOrVariable>,
            Spanned<Literal>,
            Box<Spanned<Expression>>,
        ),
    }
}

impl std::fmt::Display for System {
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
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
/// This represents built in functions that cannot be overwritten, but can be invoked by the user as functions.
pub enum SysCall {
    System(System),
    /// Represents any mathmatical function that can be called.
    Math(Math),
}

impl Documentation for SysCall {
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

impl std::fmt::Display for SysCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SysCall::System(s) => write!(f, "{}", s),
            SysCall::Math(m) => write!(f, "{}", m),
        }
    }
}

impl SysCall {
    pub fn is_syscall(identifier: &str) -> bool {
        tokenizer::token::is_syscall(identifier)
    }
}
