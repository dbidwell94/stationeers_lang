# Slang

Slang is a high-level programming language that compiles to IC10 assembly for [Stationeers](https://store.steampowered.com/app/544550/Stationeers/).

It gives you a C-like workflow while still targeting the limited IC10 instruction set used in game.

## Quick links

- [Wiki home](https://github.com/dbidwell94/stationeers_lang/wiki)
- [User getting started](https://github.com/dbidwell94/stationeers_lang/wiki#getting-started)
- [Language reference](https://github.com/dbidwell94/stationeers_lang/wiki/Language-Reference)
- [Built-in functions](https://github.com/dbidwell94/stationeers_lang/wiki/Built-In-Functions)
- [Examples](https://github.com/dbidwell94/stationeers_lang/wiki/Examples)
- [Changelog](Changelog.md)

## Why Slang

Slang is designed to reduce manual IC10 boilerplate by providing:

- Familiar control flow and expression syntax
- Named device bindings with property access
- Automatic register allocation
- Built-in function/syscall support
- Compile-time temperature units (C/F/K)

## Quick example

```rust
device gasSensor = "d0";
device airCon = "d1";

const TARGET_TEMP = 20c;

loop {
    yield();
    airCon.On = gasSensor.Temperature > TARGET_TEMP;
}
```

## Build and run

### Compile a small snippet from stdin

```bash
cd rust_compiler
echo 'let x = 5;' | cargo run --bin slang
```

### Compile with optimizer enabled

```bash
cd rust_compiler
echo 'let x = 5;' | cargo run --bin slang -- -z
```

### Compile from file

```bash
cd rust_compiler
cargo run --bin slang -- input.slang -o output.ic10
```

### Run Rust workspace tests

```bash
cd rust_compiler
cargo test --workspace --lib
```

### Full project build (Rust + C# mod packaging)

```bash
./build.sh
```

## Compiler pipeline at a glance

Slang compiles in staged passes:

1. Tokenizer converts text into tokens
2. Parser builds an AST
3. Static analysis validates semantic rules and builds a symbol table
4. Compiler emits IL/IC10 instructions
5. Optimizer rewrites output into a leaner equivalent form

## Contributor getting started

If you want to change behavior, start by opening the crate that owns that stage.

### Which crate should I edit?

- Language tokens, keywords, symbols: [rust_compiler/libs/tokenizer/README.md](rust_compiler/libs/tokenizer/README.md)
- Syntax and AST shape: [rust_compiler/libs/parser/README.md](rust_compiler/libs/parser/README.md)
- Semantic checks and symbol analysis: [rust_compiler/libs/static_analysis/src/lib.rs](rust_compiler/libs/static_analysis/src/lib.rs)
- Code generation to IL/IC10: [rust_compiler/libs/compiler/README.md](rust_compiler/libs/compiler/README.md)
- Optimization passes: [rust_compiler/libs/optimizer/README.md](rust_compiler/libs/optimizer/README.md)
- Shared instruction model (Instruction/Operand): [rust_compiler/libs/il/README.md](rust_compiler/libs/il/README.md)
- Shared helper utilities/macros/spans: [rust_compiler/libs/helpers/README.md](rust_compiler/libs/helpers/README.md)
- End-to-end pipeline regressions: [rust_compiler/libs/integration_tests/README.md](rust_compiler/libs/integration_tests/README.md)
- C# mod integration and game hooks: [csharp_mod/Plugin.cs](csharp_mod/Plugin.cs)

### Fast path for most code changes

1. Pick the owning crate from the list above
2. Add or update tests in that crate (and integration tests when behavior crosses stages)
3. Run workspace tests from rust_compiler
4. If output intentionally changed, review snapshot updates in integration_tests

## Project status

Slang is under active development.

It is intended to speed up most Stationeers scripting workflows, but for highly specialized performance-critical scripts, hand-written IC10 may still be preferred.
