use super::{Literal, LiteralOrVariable};

#[derive(Debug, PartialEq, Eq)]
pub enum Math {
    /// Gets the absolute value of a number.
    /// ## In Game
    /// `abs r? a(r?|num)`
    Abs(LiteralOrVariable),
    /// Rounds a number up to the nearest whole number.
    /// ## In Game
    /// `ceil r? a(r?|num)`
    Ceil(LiteralOrVariable),
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
    /// Computes the square root of a number.
    /// ## In Game
    /// `sqrt r? a(r?|num)`
    Sqrt(LiteralOrVariable),
    /// Gets a random number between 0 and 1.
    /// ## In Game
    /// `rand r?`
    Rand,
    /// Truncates a number by removing the decimal portion.
    /// ## In Game
    /// `trunc r? a(r?|num)`
    Trunc(LiteralOrVariable),

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

    /// Returns the cosine of the specified angle in radians.
    /// ## In Game
    /// cos r? a(r?|num)
    Cos(LiteralOrVariable),

    /// Returns the sine of the specified angle in radians.
    /// ## In Game
    /// `sin r? a(r?|num)`
    Sin(LiteralOrVariable),

    /// Returns the tangent of the specified angle in radians.
    /// ## In Game
    /// `tan r? a(r?|num)`
    Tan(LiteralOrVariable),
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
    /// Represents a function that can be called to load a variable from a device into a register.
    /// ## In Game
    /// `l r? d? logicType`
    LoadVar(Literal, Literal),
    /// Gets the in-game hash for a specific prefab name.
    /// ## In Game
    /// `HASH("prefabName")`
    Hash(LiteralOrVariable),
    /// Represents a function which will clear the stack for a provided device.
    /// ## In Game
    /// `clr d?`
    StackClear(Literal),
    /// Represents a function which reads a value from a device at a specific address and stores it in a register.
    /// ## In Game
    /// `get r? d? address(r?|num)`
    Get(Literal, LiteralOrVariable),
    /// Represents a function which loads a device variable into a register.
    /// ## In Game
    /// `l r? d? var`
    /// ## Examples
    /// `l r0 d0 Setting`
    /// `l r1 d5 Pressure`
    Load(String, LiteralOrVariable),
    /// Represents a function which stores a setting into a specific device.
    /// ## In Game
    /// `s d? logicType r?`
    /// ## Example
    /// `s d0 Setting r0`
    Store(String, LiteralOrVariable, LiteralOrVariable),
}

#[derive(Debug, PartialEq, Eq)]
/// This represents built in functions that cannot be overwritten, but can be invoked by the user as functions.
pub enum SysCall {
    System(System),
    /// Represents any mathmatical function that can be called.
    Math(Math),
}
