# Changelog

[0.3.4]

- Added support for `loadReagent`, which maps to the `lr` IC10 instruction
  - Shorthand is `lr`
  - Longform is `loadReagent`

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
