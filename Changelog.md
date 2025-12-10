# Changelog

[0.2.2]

- Fixed some formatting issues when converting Markdown to Text Mesh Pro for
  Stationpedia

[0.2.1]

- Added support for `loadSlot` and `setSlot`
- Fixed bug where syscalls like `max(1, 2)` were not allowed in assignment expressions

[0.2.0]

- Completely re-wrote the tokenizer to use `logos`
- Changed AST and Token data structures to use `Cow` instead of `String`
- Updated error reporting to use `thiserror` instead of `quickerror`

[0.1.2]

- Removed references to `Unitask`
