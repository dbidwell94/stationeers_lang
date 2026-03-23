# Changelog

[0.7.1]

- Fixed optimizer bug in `-z` builds where values used by shift/bitwise operations
  could be incorrectly treated as dead stores and removed
  - This could break bit-packing patterns like `x = x << 1` after comparisons
- Expanded optimizer register read/write tracking coverage for instruction analysis
  (`sll`, `sra`, `srl`, `nor`, `not`, `clr`, and related multi-operand syscall forms)
- Added regression tests to prevent future dead-store elimination regressions for
  shift, `not`, and `clr` usage

[0.7.0]

- Added support for `lbs` and `lbns` syscalls
- Syscall arguments now accept `const` string variables in addition to string literals
- Fixed bug where two syscalls used in the same binary expression could clobber each
  other's return register (`r15`), producing incorrect results
- Improved register forwarding optimizer to correctly handle backwards jumps (loops),
  preventing invalid optimizations across loop boundaries
- Fixed parser bug that allowed assignment expressions in `if (...)` and `while (...)`
  conditions; conditions now reject assignments and require non-assignment boolean-style expressions

[0.6.2]

- Fixed stack overflow bug with `continue`/`break` in loops with stack spill
  - When a loop body contained enough local variables to spill to stack,
    `continue` and `break` statements would not clean up the stack before jumping,
    causing unbounded stack growth and eventual stack overflow exceptions

[0.6.1]

- Update FFI Marshalling to better handle freeing Rust code
  - Eliminated a double free memory corruption bug which caused
    a crash-to-desktop in some cases

[0.6.0]

- Added support for bitwise operations with binary (0b...), hex (0x...),
  and octal (0o...) literals
- Implemented bitwise constant folding and expression optimization
- Added initial array indexing support
- Enhanced LSP integration for better IDE support
- Added beta user documentation visible directly in the IDE
- Improved symbol tracking during parse stage (variables, functions, devices)
- Enhanced constant folding for improved code generation
- Improved CLI error handling with better span information
- Updated Rust dependencies and `IC10Editor` DLL compatibility

[0.5.1]

- Fixed optimizer bug where `StoreBatch` and `StoreBatchNamed` instructions
  were not recognized as reading operands, causing incorrect elimination of
  necessary device property loads
- Added comprehensive register read tracking for `StoreSlot`, `JumpRelative`,
  and `Alias` instructions in the optimizer

[0.5.0]

- Added full tuple support: declarations, assignments, and returns
- Refactored optimizer into modular passes with improved code generation
- Enhanced peephole optimizations and pattern recognition
- Comprehensive test coverage for edge cases and error handling

[0.4.7]

- Added support for Windows CRLF endings

[0.4.6]

- Fixed bug in compiler where you were unable to assign a `const` value to
  a `let` variable

[0.4.5]

- Fixed issue where after clicking "Cancel" on the IC10 Editor, the side-by-side
  IC10 output would no longer update with highlighting or code updates.
- Added ability to live-reload the mod while developing using the `ScriptEngine`
  mod from BepInEx
  - This required adding in cleanup code to cleanup references to the Rust DLL
    before destroying the mod instance.
- Added BepInEx debug logging. This will ONLY show if you have debug logs
  enabled in the BepInEx configuration file.

[0.4.4]

- Added Stationpedia docs back after removing all harmony patches from the mod

[0.4.3]

- Removed references to the `Mod` class from SLP. This was the root of the multiplayer
  connectivity issues. Multiplayer should now work with Slang installed.

[0.4.2]

- Removed all harmony patches as most functionality as been added into the
  `IC10 Editor` mod
- IC10 runtime errors will have been reverted back to showing as IC10 line
  numbers instead of Slang line numbers.
  - The IC10 line should be easily mapped to a Slang line via the side-by-side
    IC10 compilation view.

[0.4.1]

- Update syscalls for `loadSlot` and `setSlot` to support expressions instead of
  just variables for the slot index
- Moved the main repository from GitHub to a self-hosted Gitea
  - Restructured workflow files to support this change
  - GitHub will still remain as a mirrored repository of the new
    Gitea instance.
  - This is in response to the new upcoming changes to the pricing model
    for self-hosted GitHub action runners.

[0.4.0]

- First pass getting compiled IC10 to output along side the Slang source code
  - IC10 side is currently not scrollable, and text might be cut off from the bottom,
    requiring newlines to be added to the bottom of the Slang source if needed

[0.3.4]

- Added support for `loadReagent`, which maps to the `lr` IC10 instruction
  - Shorthand is `lr`
  - Longform is `loadReagent`
- Update various Rust dependencies
- Added more optimizations, prioritizing `pop` instead of `get` when available
  when backing up / restoring registers for function invocations. This should
  save approximately 2 lines per backed up register

[0.3.3]

- Fixed bug where negative temperature literals were converted to Kelvin
  first before applying the negative

[0.3.2]

- Fixed stack overflow due to incorrect optimization of 'leaf' functions

[0.3.1]

- Fixed possible `KeyNotFoundException` in C# code due to invalid
  dictionary access when an IC housing has an error

[0.3.0]

- Implemented a multi-pass optimizer
  - This should significantly reduce line count in the final output
- Fixed source map to line up with newly optimized code

[0.2.4]

- Groundwork laid to collect and track source maps
- IC Housing will now display the `Slang` source error line (if available)
  instead of the `IC10` source error line

[0.2.3]

- Fixed stack underflow with function invocations
  - They are still "heavy", but they should work as expected now
- Fixed issue where syscall functions were not allowed as infix operators

[0.2.2]

- Fixed some formatting issues when converting Markdown to Text Mesh Pro for
  Stationpedia
- Added support for ternary expressions
  - `let i = someValue ? 4 : 5;`
  - `i = someValue ? 4 : 5;`
  - This greedily evaluates both sides, so side effects like calling functions
    is not recommended i.e.
    - `i = someValue : doSomething() : doSomethingElse();`
    - Both sides will be evaluated before calling the `select` instruction

[0.2.1]

- Added support for `loadSlot` and `setSlot`
- Fixed bug where syscalls like `max(1, 2)` were not allowed in assignment expressions

[0.2.0]

- Completely re-wrote the tokenizer to use `logos`
- Changed AST and Token data structures to use `Cow` instead of `String`
- Updated error reporting to use `thiserror` instead of `quickerror`

[0.1.2]

- Removed references to `Unitask`
