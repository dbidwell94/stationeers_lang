use super::LiteralOrVariable;

#[derive(Debug, PartialEq, Eq)]
pub enum Math {
    /// Returns the angle in radians whose cosine is the specified number.
    /// ## In Game
    /// `acos r? a(r?|num)`
    Acos(LiteralOrVariable),
    /// Returns the angle in radians whose sine is the specified number.
    /// ## In Game
    /// `asin r? a(r?|num)`
    Asin(LiteralOrVariable),
    /// Returns the angle in radians whose tangent is the specified number.
    /// ## In Game
    /// `atan r? a(r?|num)`
    Atan(LiteralOrVariable),
    /// Returns the angle in radians whose tangent is the quotient of the specified numbers.
    /// ## In Game
    /// `atan2 r? a(r?|num) b(r?|num)`
    Atan2(LiteralOrVariable, LiteralOrVariable),
    /// Gets the absolute value of a number.
    /// ## In Game
    /// `abs r? a(r?|num)`
    Abs(LiteralOrVariable),
    /// Rounds a number up to the nearest whole number.
    /// ## In Game
    /// `ceil r? a(r?|num)`
    Ceil(LiteralOrVariable),
    /// Returns the cosine of the specified angle in radians.
    /// ## In Game
    /// cos r? a(r?|num)
    Cos(LiteralOrVariable),
    /// Rounds a number down to the nearest whole number.
    /// ## In Game
    /// `floor r? a(r?|num)`
    Floor(LiteralOrVariable),
    /// Computes the natural logarithm of a number.
    /// ## In Game
    /// `log r? a(r?|num)`
    Log(LiteralOrVariable),
    /// Computes the maximum of two numbers.
    /// ## In Game
    /// `max r? a(r?|num) b(r?|num)`
    Max(LiteralOrVariable, LiteralOrVariable),
    /// Computes the minimum of two numbers.
    /// ## In Game
    /// `min r? a(r?|num) b(r?|num)`
    Min(LiteralOrVariable, LiteralOrVariable),
    /// Gets a random number between 0 and 1.
    /// ## In Game
    /// `rand r?`
    Rand,
    /// Returns the sine of the specified angle in radians.
    /// ## In Game
    /// `sin r? a(r?|num)`
    Sin(LiteralOrVariable),
    /// Computes the square root of a number.
    /// ## In Game
    /// `sqrt r? a(r?|num)`
    Sqrt(LiteralOrVariable),
    /// Returns the tangent of the specified angle in radians.
    /// ## In Game
    /// `tan r? a(r?|num)`
    Tan(LiteralOrVariable),
    /// Truncates a number by removing the decimal portion.
    /// ## In Game
    /// `trunc r? a(r?|num)`
    Trunc(LiteralOrVariable),
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

#[derive(Debug, PartialEq, Eq)]
pub enum System {
    /// Pauses execution for exactly 1 tick and then resumes.
    /// ## In Game
    /// yield
    Yield,
    /// Represents a function that can be called to sleep for a certain amount of time.
    /// ## In Game
    /// `sleep a(r?|num)`
    Sleep(LiteralOrVariable),
    /// Gets the in-game hash for a specific prefab name.
    /// ## In Game
    /// `HASH("prefabName")`
    Hash(LiteralOrVariable),
    /// Represents a function which loads a device variable into a register.
    /// ## In Game
    /// `l r? d? var`
    /// ## Examples
    /// `l r0 d0 Setting`
    /// `l r1 d5 Pressure`
    LoadFromDevice(LiteralOrVariable, String),
    /// Represents a function which stores a setting into a specific device.
    /// ## In Game
    /// `s d? logicType r?`
    /// ## Example
    /// `s d0 Setting r0`
    SetOnDevice(LiteralOrVariable, String, String),
}

impl std::fmt::Display for System {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            System::Yield => write!(f, "yield()"),
            System::Sleep(a) => write!(f, "sleep({})", a),
            System::Hash(a) => write!(f, "HASH({})", a),
            System::LoadFromDevice(a, b) => write!(f, "loadFromDevice({}, {})", a, b),
            System::SetOnDevice(a, b, c) => write!(f, "setOnDevice({}, {}, {})", a, b, c),
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
        match identifier {
            "yield" | "sleep" | "HASH" | "loadFromDevice" | "setOnDevice" => true,
            "acos" | "asin" | "atan" | "atan2" | "abs" | "ceil" | "cos" | "floor" | "log"
            | "max" | "min" | "rand" | "sin" | "sqrt" | "tan" | "trunc" => true,
            _ => false,
        }
    }
}
