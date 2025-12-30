# Changelog

[0.5.0]

- Added support for tuple types
- Added support for tuple returns from functions
- Added support for ignoring tuple values
- Fixed various compiler bugs

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
