# IL (Intermediate Language)

## What is this?

The IL crate defines the shared instruction language used between compiler stages.

Think of it as the common "contract" for generated code: the compiler writes IL, the optimizer transforms IL, and the final output writer prints IL as IC10 text.

## What does it do?

This library provides strongly-typed enums and containers for instruction data.

Core pieces include:

- `Instruction` for operation types (`Move`, `Add`, `Jump`, `Load`, etc.)
- `Operand` for values used by instructions (`Register`, `Number`, `Label`, etc.)
- `InstructionNode` for instruction + optional source span
- `Instructions` for full instruction lists, formatting, writing, and source-map support

**Example:**

```
let program = Instructions::new(vec![
    InstructionNode::new(
        Instruction::Move(Operand::Register(1), Operand::Number(5.into())),
        None,
    ),
    InstructionNode::new(
        Instruction::Add(
            Operand::Register(2),
            Operand::Register(1),
            Operand::Number(3.into()),
        ),
        None,
    ),
    InstructionNode::new(
        Instruction::Jump(Operand::Label("main".into())),
        None,
    ),
]);
```

That structure can then be optimized, mapped back to source spans, and rendered into IC10 text.

## Why is this useful?

Without a shared IL model, each stage would need custom ad-hoc formats and fragile conversion logic.

The IL crate helps by:

- Giving every stage the same typed instruction model
- Making transformations safer and easier to reason about
- Preserving source mapping metadata for diagnostics and tooling
- Centralizing IC10 text rendering in one place (`Display` implementations)

## Who should care?

- **Compiler developers** generating instruction output
- **Optimizer contributors** rewriting instruction sequences
- **Tooling contributors** working on diagnostics, spans, or output formatting

## How does it work?

At a high level, IL is a data model plus formatting helpers.

Typical flow:

- Compiler builds `InstructionNode` values from parsed expressions
- Nodes are collected into an `Instructions` list
- Optimizer passes read and rewrite that list
- Final code output uses `Display` or `Instructions::write(...)` to emit IC10 lines
- Optional span data is used to build source maps for error reporting

In short: IL is the shared language that connects "code generation" to "code optimization" and final "text output."
