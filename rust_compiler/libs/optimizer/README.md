# Optimizer

## What is this?

The optimizer is the cleanup and improvement stage that runs after code generation.

If the compiler turns Slang into working IL instructions, the optimizer makes those instructions leaner and often faster without changing what the program does.

## What does it do?

This library takes generated IL instructions and applies a series of optimization passes.

These passes remove waste, simplify math, reduce unnecessary register movement, and clean up jump flow.

**Example:**

```
Input instructions:
Instruction::Add(Operand::Register(1), Operand::Register(2), Operand::Number(0.into()))
Instruction::Move(Operand::Register(3), Operand::Register(3))

Output instructions (after optimization):
Instruction::Move(Operand::Register(1), Operand::Register(2))
```

In that example:

- Algebraic simplification turns `Add(x, y, 0)` into `Move(x, y)`
- Dead-code cleanup removes `Move(r3, r3)` as redundant

## Why is this useful?

Without optimization, compiled output still works, but it can contain extra steps that waste instruction space or execution time.

The optimizer helps by:

- Removing instructions that do nothing
- Folding constant math ahead of runtime
- Reducing extra register shuffling
- Cleaning up unreachable or redundant control-flow paths
- Resolving labels and simplifying jumps in final output

## Who should care?

- **Compiler developers** improving output quality
- **Contributors** adding or tuning optimization passes
- **Anyone** debugging generated IL size, speed, or readability

## How does it work?

The optimizer runs multiple passes over instruction lists and repeats key passes until changes stop (or a pass limit is reached).

At a high level, it does this:

- Starts with `optimize(instructions: Instructions) -> Instructions`
- Runs iterative simplification passes (constant propagation, register forwarding, algebraic simplification, and others)
- Applies cleanup passes (dead store/code elimination, redundant move removal)
- Resolves labels to line numbers
- Performs final jump cleanup after label resolution

In short: the compiler answers "what instructions are correct?" and the optimizer answers "which equivalent instruction list is better?"
