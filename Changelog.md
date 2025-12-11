# Changelog

[0.2.3]

- Fixed stack underflow with function invocations
  - They are still "heavy", but they should work as expected now

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
