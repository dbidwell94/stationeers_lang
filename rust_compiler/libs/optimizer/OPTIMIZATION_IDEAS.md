# Additional Optimization Opportunities for Slang IL Optimizer

## Currently Implemented ✓

1. Constant Propagation - Folds math operations with known values
2. Register Forwarding - Eliminates intermediate moves
3. Function Call Optimization - Removes unnecessary push/pop around calls
4. Leaf Function Optimization - Removes RA save/restore for non-calling functions
5. Redundant Move Elimination - Removes `move rx rx`
6. Dead Code Elimination - Removes unreachable code after jumps

## Proposed Additional Optimizations

### 1. **Algebraic Simplification** 🔥 HIGH IMPACT

Simplify mathematical identities:

- `x + 0` → `x` (move)
- `x - 0` → `x` (move)
- `x * 1` → `x` (move)
- `x * 0` → `0` (move to constant)
- `x / 1` → `x` (move)
- `x - x` → `0` (move to constant)
- `x % 1` → `0` (move to constant)

**Example:**

```
add r1 r2 0     →    move r1 r2
mul r3 r4 1     →    move r3 r4
mul r5 r6 0     →    move r5 0
```

### 2. **Strength Reduction** 🔥 HIGH IMPACT

Replace expensive operations with cheaper ones:

- `x * 2` → `add x x x` (addition is cheaper than multiplication)
- `x * power_of_2` → bit shifts (if IC10 supports)
- `x / 2` → bit shifts (if IC10 supports)

**Example:**

```
mul r1 r2 2     →    add r1 r2 r2
```

### 3. **Peephole Optimization - Instruction Sequences** 🔥 MEDIUM-HIGH IMPACT

Recognize and optimize common instruction patterns:

#### Pattern: Conditional Branch Simplification

```
seq r1 ra rb     →    beq ra rb label
beqz r1 label         (remove the seq entirely)

sne r1 ra rb     →    bne ra rb label
beqz r1 label         (remove the sne entirely)
```

#### Pattern: Double Move Elimination

```
move r1 r2      →    move r1 r3
move r1 r3          (remove first move if r1 not used between)
```

#### Pattern: Redundant Load Elimination

If a register's value is already loaded and hasn't been clobbered:

```
l r1 d0 Temperature
... (no writes to r1)
l r1 d0 Temperature   →  (remove second load)
```

### 4. **Copy Propagation Enhancement** 🔥 MEDIUM IMPACT

Current register forwarding is good, but we can extend it:

- Track `move` chains: if `r1 = r2` and `r2 = 5`, propagate the `5` directly
- Eliminate the intermediate register if possible

### 5. **Dead Store Elimination** 🔥 MEDIUM IMPACT

Remove writes to registers that are never read before being overwritten:

```
move r1 5
move r1 10      →    move r1 10
                     (first write is dead)
```

### 6. **Common Subexpression Elimination (CSE)** 🔥 MEDIUM-HIGH IMPACT

Recognize when the same computation is done multiple times:

```
add r1 r8 r9
add r2 r8 r9    →    add r1 r8 r9
                     move r2 r1
```

This is especially valuable for expensive operations like:

- Device loads (`l`)
- Math functions (sqrt, sin, cos, etc.)

### 7. **Jump Threading** 🔥 LOW-MEDIUM IMPACT

Optimize jump-to-jump sequences:

```
j label1
...
label1:
j label2        →    j label2 (rewrite first jump)
```

### 8. **Branch Folding** 🔥 LOW-MEDIUM IMPACT

Merge consecutive branches to the same target:

```
bgt r1 r2 label
bgt r3 r4 label  →   Could potentially be optimized based on conditions
```

### 9. **Loop Invariant Code Motion** 🔥 MEDIUM-HIGH IMPACT

Move calculations out of loops if they don't change:

```
loop:
  mul r2 5 10      →   mul r2 5 10      (hoisted before loop)
  add r1 r1 r2         loop:
  ...                    add r1 r1 r2
  j loop                 ...
                        j loop
```

### 10. **Select Instruction Optimization** 🔥 LOW-MEDIUM IMPACT

The `select` instruction can sometimes replace branch patterns:

```
beq r1 r2 else
move r3 r4
j end
else:
move r3 r5        →   seq r6 r1 r2
end:                   select r3 r6 r5 r4
```

### 11. **Stack Access Pattern Optimization** 🔥 LOW IMPACT

If we see repeated `sub r0 sp N` + `get`, we might be able to optimize by:

- Caching the stack address in a register if used multiple times
- Combining sequential gets from adjacent stack slots

### 12. **Inline Small Functions** 🔥 HIGH IMPACT (Complex to implement)

For very small leaf functions (1-2 instructions), inline them at the call site:

```
calculateSum:
  add r15 r8 r9
  j ra

main:
  push 5           →   main:
  push 10                add r15 5 10
  jal calculateSum
```

### 13. **Branch Prediction Hints** 🔥 LOW IMPACT

Reorganize code to put likely branches inline (fall-through) and unlikely branches as jumps.

### 14. **Register Coalescing** 🔥 MEDIUM IMPACT

Reduce register pressure by reusing registers that have non-overlapping lifetimes.

## Priority Implementation Order

### Phase 1 (Quick Wins):

1. Algebraic Simplification (easy, high impact)
2. Strength Reduction (easy, high impact)
3. Dead Store Elimination (medium complexity, good impact)

### Phase 2 (Medium Effort):

4. Peephole Optimizations - seq/beq pattern (medium, high impact)
5. Common Subexpression Elimination (medium, high impact)
6. Copy Propagation Enhancement (medium, medium impact)

### Phase 3 (Advanced):

7. Loop Invariant Code Motion (complex, high impact for loop-heavy code)
8. Function Inlining (complex, high impact)
9. Register Coalescing (complex, medium impact)

## Testing Strategy

- Add test cases for each optimization
- Ensure optimization preserves semantics (run existing tests after each)
- Measure code size reduction
- Consider adding benchmarks to measure game performance impact
