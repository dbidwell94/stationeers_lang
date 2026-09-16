# Compiler

## What is this?

The compiler is the stage that turns parsed Slang code into IC10 instructions the game can run.

If the tokenizer identifies words and the parser understands sentence structure, the compiler is the part that actually translates meaning into executable steps.

## What does it do?

This library takes the parser's expression tree and generates low-level IC10 instruction output.

It also decides where values should live (registers or stack), tracks scopes, handles control flow (like `if`, `while`, `loop`), and records useful metadata for tooling.

**Example:**

```
Input shape: let x = (5 + 3)
Output idea:
Instruction::Move(Operand::Register(1), Operand::Number(5.into()))
Instruction::Move(Operand::Register(2), Operand::Number(3.into()))
Instruction::Add(Operand::Register(8), Operand::Register(1), Operand::Register(2))
```

The exact instruction text may vary, but the goal is always the same: convert high-level Slang intent into valid IC10 operations.

## Why is this useful?

Without the compiler stage, parsed code would just be a structured description with no executable result.

The compiler is what makes Slang practical by:

- Producing runnable IC10 instructions
- Managing limited registers automatically
- Handling stack usage when registers run out
- Enforcing language rules during generation
- Returning helpful compile errors with source locations

## Who should care?

- **Compiler developers** working on code generation
- **Contributors** adding language features that need emitted instructions
- **Anyone** debugging wrong output, register usage, or compile-time errors

## How does it work?

At a high level, the compiler walks the parsed tree and emits instructions as it goes.

It generally follows this flow:

- Starts from the root expression block
- Visits each expression type (`declaration`, `assignment`, `if`, `loop`, function calls, etc.)
- Resolves operands (literal values, variables, device access)
- Allocates storage locations (temporary registers, persistent registers, or stack)
- Emits IC10 IL instructions in execution order
- Collects errors and metadata while compiling

In short: the parser answers "what does this code mean?" and the compiler answers "what exact IC10 steps should run?"
