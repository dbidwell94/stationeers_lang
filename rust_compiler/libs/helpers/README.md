# Helpers

## What is this?

The helpers crate is a shared utility toolbox for the compiler workspace.

It contains small, reusable building blocks that multiple crates rely on, so common logic does not need to be rewritten everywhere.

## What does it do?

This library provides utility types, helper functions, and macros used across tokenizer, parser, compiler, and related tooling.

Core pieces include:

- Span for source location tracking (line and column ranges)
- Conversion from Span to LSP Range for editor diagnostics
- Utility functions like crc_hash_signed and dedent
- Shared macros like documented! and with_syscalls!
- A prelude module for convenient shared imports

**Example:**

```rust
let span = Span {
    start_line: 3,
    start_col: 4,
    end_line: 3,
    end_col: 12,
};

let lsp_range: lsp_types::Range = span.into();
```

```rust
let value = crc_hash_signed("StructureSolarPanelDual");
```

These helpers are intentionally small, but they remove a lot of repeated boilerplate across the workspace.

## Why is this useful?

Without a shared helpers crate, each stage would duplicate the same support code for spans, docs, hashing, and macro logic.

The helpers crate improves the workspace by:

- Keeping common logic in one place
- Making diagnostics consistent across crates
- Reducing copy-paste utility code
- Standardizing recurring patterns with reusable macros

## Who should care?

- Compiler developers using shared span and utility logic
- Contributors adding enum docs or syscall lists
- Tooling developers integrating diagnostics and documentation

## How does it work?

The crate exposes a small public API that other crates import directly.

Typical usage flow:

- Parser/tokenizer/compiler create and pass Span values through their pipelines
- Error conversion code turns Span into lsp_types::Range
- Macros (for example documented!) generate documentation behavior for enums
- with_syscalls! injects the canonical syscall name list where needed
- prelude re-exports common helper items for cleaner imports

In short: helpers is the shared support layer that keeps the rest of the compiler workspace clean and consistent.
