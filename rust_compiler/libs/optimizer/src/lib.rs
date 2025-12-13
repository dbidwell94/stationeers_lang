use il::{Instruction, InstructionNode, Operand};
use rust_decimal::Decimal;
use std::collections::{HashMap, HashSet};

mod leaf_function;
use leaf_function::find_leaf_functions;

/// Entry point for the optimizer.
pub fn optimize<'a>(mut instructions: Vec<InstructionNode<'a>>) -> Vec<InstructionNode<'a>> {
    let mut changed = true;
    let mut pass_count = 0;
    const MAX_PASSES: usize = 10;

    // Iterative passes for code simplification
    while changed && pass_count < MAX_PASSES {
        changed = false;
        pass_count += 1;

        // Pass 1: Constant Propagation
        let (new_inst, c1) = constant_propagation(instructions);
        instructions = new_inst;
        changed |= c1;

        // Pass 2: Register Forwarding (Intermediate Move Elimination)
        let (new_inst, c2) = register_forwarding(instructions);
        instructions = new_inst;
        changed |= c2;

        // Pass 3: Function Call Optimization (Remove unused push/pop around calls)
        let (new_inst, c3) = optimize_function_calls(instructions);
        instructions = new_inst;
        changed |= c3;

        // Pass 4: Leaf Function Optimization (Remove RA save/restore for leaf functions)
        // This is separate from pass 3 as it deals with the function *definition*, not the call site.
        let (new_inst, c4) = optimize_leaf_functions(instructions);
        instructions = new_inst;
        changed |= c4;

        // Pass 5: Redundant Move Elimination
        let (new_inst, c5) = remove_redundant_moves(instructions);
        instructions = new_inst;
        changed |= c5;

        // Pass 6: Dead Code Elimination
        let (new_inst, c6) = remove_unreachable_code(instructions);
        instructions = new_inst;
        changed |= c6;
    }

    // Final Pass: Resolve Labels to Line Numbers
    resolve_labels(instructions)
}

/// Pass: Leaf Function Optimization
/// If a function makes no calls (is a leaf), it doesn't need to save/restore `ra`.
fn optimize_leaf_functions<'a>(
    mut input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let leaves = find_leaf_functions(&input);
    if leaves.is_empty() {
        return (input, false);
    }

    let mut changed = false;
    let mut to_remove = HashSet::new();
    let mut current_function: Option<String> = None;

    // Map of FunctionName -> The stack offset where RA was stored.
    // We need this to adjust other stack accesses (arguments vs locals).
    let mut func_ra_offsets = HashMap::new();

    // First scan: Identify instructions to remove and capture RA offsets
    for (i, node) in input.iter().enumerate() {
        match &node.instruction {
            Instruction::LabelDef(label) => {
                current_function = Some(label.to_string());
            }
            Instruction::Push(Operand::ReturnAddress) => {
                if let Some(func) = &current_function {
                    if leaves.contains(func) {
                        to_remove.insert(i);
                        changed = true;
                    }
                }
            }
            Instruction::Get(Operand::ReturnAddress, _, Operand::Register(_)) => {
                // This is the restore instruction: `get ra db r0`
                if let Some(func) = &current_function {
                    if leaves.contains(func) {
                        to_remove.insert(i);
                        // Look back for the address calc: `sub r0 sp OFFSET`
                        if i > 0 {
                            if let Instruction::Sub(_, Operand::StackPointer, Operand::Number(n)) =
                                &input[i - 1].instruction
                            {
                                func_ra_offsets.insert(func.clone(), *n);
                                to_remove.insert(i - 1);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    if !changed {
        return (input, false);
    }

    // Second scan: Rebuild with adjustments
    let mut output = Vec::with_capacity(input.len());
    let mut processing_function: Option<String> = None;

    for (i, mut node) in input.into_iter().enumerate() {
        if to_remove.contains(&i) {
            continue;
        }

        if let Instruction::LabelDef(l) = &node.instruction {
            processing_function = Some(l.to_string());
        }

        // Apply Stack Adjustments if we are inside a leaf function that we optimized
        if let Some(func) = &processing_function {
            if let Some(ra_offset) = func_ra_offsets.get(func) {
                // If this is the stack cleanup `sub sp sp N`, decrement N by 1 (since we removed push ra)
                if let Instruction::Sub(
                    Operand::StackPointer,
                    Operand::StackPointer,
                    Operand::Number(n),
                ) = &mut node.instruction
                {
                    let new_n = *n - Decimal::from(1);
                    if new_n.is_zero() {
                        continue; // Remove instruction if 0
                    }
                    *n = new_n;
                }

                // Adjust stack variable accesses relative to the removed RA.
                // Compiler layout: [Args] [RA] [Locals/Temps]
                // Stack grows up (increment sp on push).
                // Access is `sp - offset`.
                // Deeper items (Args) have LARGER offsets than RA.
                // Shallower items (Locals) have SMALLER offsets than RA.
                // Since RA is gone, items deeper than RA (Args) effectively shift "down" (index - 1).
                if let Instruction::Sub(_, Operand::StackPointer, Operand::Number(n)) =
                    &mut node.instruction
                {
                    if *n > *ra_offset {
                        *n -= Decimal::from(1);
                    }
                }
            }
        }

        output.push(node);
    }

    (output, true)
}

/// Analyzes which registers are written to by each function label.
fn analyze_clobbers(instructions: &[InstructionNode]) -> HashMap<String, HashSet<u8>> {
    let mut clobbers = HashMap::new();
    let mut current_label = None;

    for node in instructions {
        if let Instruction::LabelDef(label) = &node.instruction {
            current_label = Some(label.to_string());
            clobbers.insert(label.to_string(), HashSet::new());
        }

        if let Some(label) = &current_label {
            if let Some(reg) = get_destination_reg(&node.instruction) {
                if let Some(set) = clobbers.get_mut(label) {
                    set.insert(reg);
                }
            }

            // Note: If we call another function, we technically clobber whatever IT clobbers
            // (unless we save/restore it, which counts as a write anyway).
            // This simple pass relies on the fact that any register modification (including restore) is a 'write'.
        }
    }
    clobbers
}

/// Pass: Function Call Optimization
/// Removes Push/Restore pairs surrounding a JAL if the target function does not clobber that register.
fn optimize_function_calls<'a>(
    mut input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let clobbers = analyze_clobbers(&input);
    let mut changed = false;
    let mut to_remove = HashSet::new();
    let mut stack_adjustments = HashMap::new(); // Index of `sub sp sp N` -> amount to subtract

    let mut i = 0;
    while i < input.len() {
        if let Instruction::JumpAndLink(Operand::Label(target)) = &input[i].instruction {
            let target_key = target.to_string();
            // If we don't have info on the function (e.g. extern or complex), skip
            if let Some(func_clobbers) = clobbers.get(&target_key) {
                // 1. Identify Pushes immediately preceding the JAL
                let mut pushes = Vec::new(); // (index, register)
                let mut scan_back = i.saturating_sub(1);
                while scan_back > 0 {
                    if to_remove.contains(&scan_back) {
                        scan_back -= 1;
                        continue;
                    }
                    if let Instruction::Push(Operand::Register(r)) = &input[scan_back].instruction {
                        pushes.push((scan_back, *r));
                        scan_back -= 1;
                    } else {
                        break;
                    }
                }

                // 2. Identify Restores immediately following the JAL
                // Compiler emits: sub r0 sp Offset, get Reg db r0.
                let mut restores = Vec::new(); // (index_of_get, register, index_of_sub)
                let mut scan_fwd = i + 1;
                while scan_fwd < input.len() {
                    // Skip over the 'sub r0 sp X' address calculation lines
                    if let Instruction::Sub(Operand::Register(0), Operand::StackPointer, _) =
                        &input[scan_fwd].instruction
                    {
                        // Check next instruction for the Get
                        if scan_fwd + 1 < input.len() {
                            if let Instruction::Get(Operand::Register(r), _, Operand::Register(0)) =
                                &input[scan_fwd + 1].instruction
                            {
                                restores.push((scan_fwd + 1, *r, scan_fwd));
                                scan_fwd += 2;
                                continue;
                            }
                        }
                    }
                    break;
                }

                // 3. Check for Stack Cleanup `sub sp sp N`
                let cleanup_idx = scan_fwd;

                let has_cleanup = if cleanup_idx < input.len() {
                    if let Instruction::Sub(
                        Operand::StackPointer,
                        Operand::StackPointer,
                        Operand::Number(_),
                    ) = &input[cleanup_idx].instruction
                    {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

                // "All or Nothing" strategy for the safe subset:
                let all_pushes_safe = pushes.iter().all(|(_, r)| !func_clobbers.contains(r));

                let push_set: HashSet<u8> = pushes.iter().map(|(_, r)| *r).collect();
                let restore_set: HashSet<u8> = restores.iter().map(|(_, r, _)| *r).collect();

                if all_pushes_safe && has_cleanup && push_set == restore_set {
                    // We can remove ALL saves/restores for this call!
                    for (p_idx, _) in pushes {
                        to_remove.insert(p_idx);
                    }
                    for (g_idx, _, s_idx) in restores {
                        to_remove.insert(g_idx);
                        to_remove.insert(s_idx);
                    }

                    // Reduce stack cleanup amount
                    let num_removed = push_set.len() as i64;
                    stack_adjustments.insert(cleanup_idx, num_removed);
                    changed = true;
                }
            }
        }
        i += 1;
    }

    if changed {
        let mut clean = Vec::with_capacity(input.len());
        for (idx, mut node) in input.into_iter().enumerate() {
            if to_remove.contains(&idx) {
                continue;
            }

            // Apply stack adjustment
            if let Some(reduction) = stack_adjustments.get(&idx) {
                if let Instruction::Sub(dst, a, Operand::Number(n)) = &node.instruction {
                    let new_n = n - Decimal::from(*reduction);
                    if new_n.is_zero() {
                        continue; // Remove the sub entirely if 0
                    }
                    node.instruction =
                        Instruction::Sub(dst.clone(), a.clone(), Operand::Number(new_n));
                }
            }

            clean.push(node);
        }
        return (clean, changed);
    }

    (input, false)
}

/// Pass: Register Forwarding
/// Eliminates intermediate moves by writing directly to the final destination.
/// Example: `l r1 d0 T` + `move r9 r1` -> `l r9 d0 T`
fn register_forwarding<'a>(
    mut input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut changed = false;
    let mut i = 0;

    // We use a while loop to manually control index so we can peek ahead
    while i < input.len().saturating_sub(1) {
        let next_idx = i + 1;

        // Check if current instruction defines a register
        // and the NEXT instruction is a move from that register.
        let forward_candidate = if let Some(def_reg) = get_destination_reg(&input[i].instruction) {
            if let Instruction::Move(Operand::Register(dest_reg), Operand::Register(src_reg)) =
                &input[next_idx].instruction
            {
                if *src_reg == def_reg {
                    // Candidate found: Instruction `i` defines `src_reg`, Instruction `i+1` moves `src_reg` to `dest_reg`.
                    // We can optimize if `src_reg` (the temp) is NOT used after this move.
                    Some((def_reg, *dest_reg))
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        if let Some((temp_reg, final_reg)) = forward_candidate {
            // Check liveness: Is temp_reg used after i+1?
            // We scan from i+2 onwards.
            let mut temp_is_dead = true;
            for node in input.iter().skip(i + 2) {
                if reg_is_read(&node.instruction, temp_reg) {
                    temp_is_dead = false;
                    break;
                }
                // If the temp is redefined, then the old value is dead, so we are safe.
                if let Some(redef) = get_destination_reg(&node.instruction) {
                    if redef == temp_reg {
                        break;
                    }
                }
                // If we hit a label/jump, we assume liveness might leak (conservative safety)
                if matches!(
                    node.instruction,
                    Instruction::LabelDef(_) | Instruction::Jump(_) | Instruction::JumpAndLink(_)
                ) {
                    temp_is_dead = false;
                    break;
                }
            }

            if temp_is_dead {
                // Perform the swap
                // 1. Rewrite input[i] to write to final_reg
                if let Some(new_instr) = set_destination_reg(&input[i].instruction, final_reg) {
                    input[i].instruction = new_instr;
                    // 2. Remove input[i+1] (The Move)
                    input.remove(next_idx);
                    changed = true;
                    // Don't increment i, re-evaluate current index (which is now a new neighbor)
                    continue;
                }
            }
        }

        i += 1;
    }

    (input, changed)
}

/// Pass: Resolve Labels
/// Converts all Jump/Branch labels to absolute line numbers and removes LabelDefs.
fn resolve_labels<'a>(input: Vec<InstructionNode<'a>>) -> Vec<InstructionNode<'a>> {
    let mut label_map: HashMap<String, usize> = HashMap::new();
    let mut line_number = 0;

    // 1. Build Label Map (filtering out LabelDefs from the count)
    for node in &input {
        if let Instruction::LabelDef(name) = &node.instruction {
            label_map.insert(name.to_string(), line_number);
        } else {
            line_number += 1;
        }
    }

    let mut output = Vec::with_capacity(input.len());

    // 2. Rewrite Jumps and Filter Labels
    for mut node in input {
        // Helper to get line number as Decimal operand
        let get_line = |lbl: &Operand| -> Option<Operand<'a>> {
            if let Operand::Label(name) = lbl {
                label_map
                    .get(name.as_ref())
                    .map(|&l| Operand::Number(Decimal::from(l)))
            } else {
                None
            }
        };

        match &mut node.instruction {
            Instruction::LabelDef(_) => continue, // Strip labels

            // Jumps
            Instruction::Jump(op) => {
                if let Some(num) = get_line(op) {
                    *op = num;
                }
            }
            Instruction::JumpAndLink(op) => {
                if let Some(num) = get_line(op) {
                    *op = num;
                }
            }
            Instruction::BranchEq(a, b, op)
            | Instruction::BranchNe(a, b, op)
            | Instruction::BranchGt(a, b, op)
            | Instruction::BranchLt(a, b, op)
            | Instruction::BranchGe(a, b, op)
            | Instruction::BranchLe(a, b, op) => {
                if let Some(num) = get_line(op) {
                    *op = num;
                }
            }
            Instruction::BranchEqZero(a, op) | Instruction::BranchNeZero(a, op) => {
                if let Some(num) = get_line(op) {
                    *op = num;
                }
            }
            _ => {}
        }
        output.push(node);
    }

    output
}

// --- Helpers for Register Analysis ---

fn get_destination_reg(instr: &Instruction) -> Option<u8> {
    match instr {
        Instruction::Move(Operand::Register(r), _)
        | Instruction::Add(Operand::Register(r), _, _)
        | Instruction::Sub(Operand::Register(r), _, _)
        | Instruction::Mul(Operand::Register(r), _, _)
        | Instruction::Div(Operand::Register(r), _, _)
        | Instruction::Mod(Operand::Register(r), _, _)
        | Instruction::Pow(Operand::Register(r), _, _)
        | Instruction::Load(Operand::Register(r), _, _)
        | Instruction::LoadSlot(Operand::Register(r), _, _, _)
        | Instruction::LoadBatch(Operand::Register(r), _, _, _)
        | Instruction::LoadBatchNamed(Operand::Register(r), _, _, _, _)
        | Instruction::SetEq(Operand::Register(r), _, _)
        | Instruction::SetNe(Operand::Register(r), _, _)
        | Instruction::SetGt(Operand::Register(r), _, _)
        | Instruction::SetLt(Operand::Register(r), _, _)
        | Instruction::SetGe(Operand::Register(r), _, _)
        | Instruction::SetLe(Operand::Register(r), _, _)
        | Instruction::And(Operand::Register(r), _, _)
        | Instruction::Or(Operand::Register(r), _, _)
        | Instruction::Xor(Operand::Register(r), _, _)
        | Instruction::Peek(Operand::Register(r))
        | Instruction::Get(Operand::Register(r), _, _)
        | Instruction::Select(Operand::Register(r), _, _, _)
        | Instruction::Rand(Operand::Register(r))
        | Instruction::Acos(Operand::Register(r), _)
        | Instruction::Asin(Operand::Register(r), _)
        | Instruction::Atan(Operand::Register(r), _)
        | Instruction::Atan2(Operand::Register(r), _, _)
        | Instruction::Abs(Operand::Register(r), _)
        | Instruction::Ceil(Operand::Register(r), _)
        | Instruction::Cos(Operand::Register(r), _)
        | Instruction::Floor(Operand::Register(r), _)
        | Instruction::Log(Operand::Register(r), _)
        | Instruction::Max(Operand::Register(r), _, _)
        | Instruction::Min(Operand::Register(r), _, _)
        | Instruction::Sin(Operand::Register(r), _)
        | Instruction::Sqrt(Operand::Register(r), _)
        | Instruction::Tan(Operand::Register(r), _)
        | Instruction::Trunc(Operand::Register(r), _)
        | Instruction::Pop(Operand::Register(r)) => Some(*r),
        _ => None,
    }
}

fn set_destination_reg<'a>(instr: &Instruction<'a>, new_reg: u8) -> Option<Instruction<'a>> {
    // Helper to easily recreate instruction with new dest
    let r = Operand::Register(new_reg);
    match instr {
        Instruction::Move(_, b) => Some(Instruction::Move(r, b.clone())),
        Instruction::Add(_, a, b) => Some(Instruction::Add(r, a.clone(), b.clone())),
        Instruction::Sub(_, a, b) => Some(Instruction::Sub(r, a.clone(), b.clone())),
        Instruction::Mul(_, a, b) => Some(Instruction::Mul(r, a.clone(), b.clone())),
        Instruction::Div(_, a, b) => Some(Instruction::Div(r, a.clone(), b.clone())),
        Instruction::Mod(_, a, b) => Some(Instruction::Mod(r, a.clone(), b.clone())),
        Instruction::Pow(_, a, b) => Some(Instruction::Pow(r, a.clone(), b.clone())),
        Instruction::Load(_, a, b) => Some(Instruction::Load(r, a.clone(), b.clone())),
        Instruction::LoadSlot(_, a, b, c) => {
            Some(Instruction::LoadSlot(r, a.clone(), b.clone(), c.clone()))
        }
        Instruction::LoadBatch(_, a, b, c) => {
            Some(Instruction::LoadBatch(r, a.clone(), b.clone(), c.clone()))
        }
        Instruction::LoadBatchNamed(_, a, b, c, d) => Some(Instruction::LoadBatchNamed(
            r,
            a.clone(),
            b.clone(),
            c.clone(),
            d.clone(),
        )),
        Instruction::SetEq(_, a, b) => Some(Instruction::SetEq(r, a.clone(), b.clone())),
        Instruction::SetNe(_, a, b) => Some(Instruction::SetNe(r, a.clone(), b.clone())),
        Instruction::SetGt(_, a, b) => Some(Instruction::SetGt(r, a.clone(), b.clone())),
        Instruction::SetLt(_, a, b) => Some(Instruction::SetLt(r, a.clone(), b.clone())),
        Instruction::SetGe(_, a, b) => Some(Instruction::SetGe(r, a.clone(), b.clone())),
        Instruction::SetLe(_, a, b) => Some(Instruction::SetLe(r, a.clone(), b.clone())),
        Instruction::And(_, a, b) => Some(Instruction::And(r, a.clone(), b.clone())),
        Instruction::Or(_, a, b) => Some(Instruction::Or(r, a.clone(), b.clone())),
        Instruction::Xor(_, a, b) => Some(Instruction::Xor(r, a.clone(), b.clone())),
        Instruction::Peek(_) => Some(Instruction::Peek(r)),
        Instruction::Get(_, a, b) => Some(Instruction::Get(r, a.clone(), b.clone())),
        Instruction::Select(_, a, b, c) => {
            Some(Instruction::Select(r, a.clone(), b.clone(), c.clone()))
        }
        Instruction::Rand(_) => Some(Instruction::Rand(r)),
        Instruction::Pop(_) => Some(Instruction::Pop(r)),

        // Math funcs
        Instruction::Acos(_, a) => Some(Instruction::Acos(r, a.clone())),
        Instruction::Asin(_, a) => Some(Instruction::Asin(r, a.clone())),
        Instruction::Atan(_, a) => Some(Instruction::Atan(r, a.clone())),
        Instruction::Atan2(_, a, b) => Some(Instruction::Atan2(r, a.clone(), b.clone())),
        Instruction::Abs(_, a) => Some(Instruction::Abs(r, a.clone())),
        Instruction::Ceil(_, a) => Some(Instruction::Ceil(r, a.clone())),
        Instruction::Cos(_, a) => Some(Instruction::Cos(r, a.clone())),
        Instruction::Floor(_, a) => Some(Instruction::Floor(r, a.clone())),
        Instruction::Log(_, a) => Some(Instruction::Log(r, a.clone())),
        Instruction::Max(_, a, b) => Some(Instruction::Max(r, a.clone(), b.clone())),
        Instruction::Min(_, a, b) => Some(Instruction::Min(r, a.clone(), b.clone())),
        Instruction::Sin(_, a) => Some(Instruction::Sin(r, a.clone())),
        Instruction::Sqrt(_, a) => Some(Instruction::Sqrt(r, a.clone())),
        Instruction::Tan(_, a) => Some(Instruction::Tan(r, a.clone())),
        Instruction::Trunc(_, a) => Some(Instruction::Trunc(r, a.clone())),

        _ => None,
    }
}

fn reg_is_read(instr: &Instruction, reg: u8) -> bool {
    let check = |op: &Operand| matches!(op, Operand::Register(r) if *r == reg);

    match instr {
        Instruction::Move(_, a) => check(a),
        Instruction::Add(_, a, b)
        | Instruction::Sub(_, a, b)
        | Instruction::Mul(_, a, b)
        | Instruction::Div(_, a, b)
        | Instruction::Mod(_, a, b)
        | Instruction::Pow(_, a, b) => check(a) || check(b),

        Instruction::Load(_, a, _) => check(a), // Load reads device? Device can be reg? Yes.
        Instruction::Store(a, _, b) => check(a) || check(b),

        Instruction::BranchEq(a, b, _)
        | Instruction::BranchNe(a, b, _)
        | Instruction::BranchGt(a, b, _)
        | Instruction::BranchLt(a, b, _)
        | Instruction::BranchGe(a, b, _)
        | Instruction::BranchLe(a, b, _) => check(a) || check(b),

        Instruction::BranchEqZero(a, _) | Instruction::BranchNeZero(a, _) => check(a),

        Instruction::SetEq(_, a, b)
        | Instruction::SetNe(_, a, b)
        | Instruction::SetGt(_, a, b)
        | Instruction::SetLt(_, a, b)
        | Instruction::SetGe(_, a, b)
        | Instruction::SetLe(_, a, b)
        | Instruction::And(_, a, b)
        | Instruction::Or(_, a, b)
        | Instruction::Xor(_, a, b) => check(a) || check(b),

        Instruction::Push(a) => check(a),
        Instruction::Get(_, a, b) => check(a) || check(b),
        Instruction::Put(a, b, c) => check(a) || check(b) || check(c),

        Instruction::Select(_, a, b, c) => check(a) || check(b) || check(c),
        Instruction::Sleep(a) => check(a),

        // Math single arg
        Instruction::Acos(_, a)
        | Instruction::Asin(_, a)
        | Instruction::Atan(_, a)
        | Instruction::Abs(_, a)
        | Instruction::Ceil(_, a)
        | Instruction::Cos(_, a)
        | Instruction::Floor(_, a)
        | Instruction::Log(_, a)
        | Instruction::Sin(_, a)
        | Instruction::Sqrt(_, a)
        | Instruction::Tan(_, a)
        | Instruction::Trunc(_, a) => check(a),

        // Math double arg
        Instruction::Atan2(_, a, b) | Instruction::Max(_, a, b) | Instruction::Min(_, a, b) => {
            check(a) || check(b)
        }

        _ => false,
    }
}

// --- Constant Propagation & Dead Code (Same as before) ---

fn constant_propagation<'a>(input: Vec<InstructionNode<'a>>) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    let mut registers: [Option<Decimal>; 16] = [None; 16];

    for mut node in input {
        match &node.instruction {
            Instruction::LabelDef(_) | Instruction::JumpAndLink(_) => registers = [None; 16],
            _ => {}
        }

        let simplified = match &node.instruction {
            Instruction::Move(dst, src) => {
                if let Some(val) = resolve_value(src, &registers) {
                    Some(Instruction::Move(dst.clone(), Operand::Number(val)))
                } else {
                    None
                }
            }
            Instruction::Add(dst, a, b) => try_fold_math(dst, a, b, &registers, |x, y| x + y),
            Instruction::Sub(dst, a, b) => try_fold_math(dst, a, b, &registers, |x, y| x - y),
            Instruction::Mul(dst, a, b) => try_fold_math(dst, a, b, &registers, |x, y| x * y),
            Instruction::Div(dst, a, b) => {
                try_fold_math(
                    dst,
                    a,
                    b,
                    &registers,
                    |x, y| if y.is_zero() { x } else { x / y },
                )
            }
            Instruction::Mod(dst, a, b) => {
                try_fold_math(
                    dst,
                    a,
                    b,
                    &registers,
                    |x, y| if y.is_zero() { x } else { x % y },
                )
            }
            Instruction::BranchEq(a, b, l) => {
                try_resolve_branch(a, b, l, &registers, |x, y| x == y)
            }
            Instruction::BranchNe(a, b, l) => {
                try_resolve_branch(a, b, l, &registers, |x, y| x != y)
            }
            Instruction::BranchGt(a, b, l) => try_resolve_branch(a, b, l, &registers, |x, y| x > y),
            Instruction::BranchLt(a, b, l) => try_resolve_branch(a, b, l, &registers, |x, y| x < y),
            Instruction::BranchGe(a, b, l) => {
                try_resolve_branch(a, b, l, &registers, |x, y| x >= y)
            }
            Instruction::BranchLe(a, b, l) => {
                try_resolve_branch(a, b, l, &registers, |x, y| x <= y)
            }
            Instruction::BranchEqZero(a, l) => {
                try_resolve_branch(a, &Operand::Number(0.into()), l, &registers, |x, y| x == y)
            }
            Instruction::BranchNeZero(a, l) => {
                try_resolve_branch(a, &Operand::Number(0.into()), l, &registers, |x, y| x != y)
            }
            _ => None,
        };

        if let Some(new) = simplified {
            node.instruction = new;
            changed = true;
        }

        // Update tracking
        match &node.instruction {
            Instruction::Move(Operand::Register(r), src) => {
                registers[*r as usize] = resolve_value(src, &registers)
            }
            // Invalidate if destination is register
            _ => {
                if let Some(r) = get_destination_reg(&node.instruction) {
                    registers[r as usize] = None;
                }
            }
        }

        // Filter out NOPs (Empty LabelDefs from branch resolution)
        if let Instruction::LabelDef(l) = &node.instruction {
            if l.is_empty() {
                changed = true;
                continue;
            }
        }

        output.push(node);
    }
    (output, changed)
}

fn resolve_value(op: &Operand, regs: &[Option<Decimal>; 16]) -> Option<Decimal> {
    match op {
        Operand::Number(n) => Some(*n),
        Operand::Register(r) => regs[*r as usize],
        _ => None,
    }
}

fn try_fold_math<'a, F>(
    dst: &Operand<'a>,
    a: &Operand<'a>,
    b: &Operand<'a>,
    regs: &[Option<Decimal>; 16],
    op: F,
) -> Option<Instruction<'a>>
where
    F: Fn(Decimal, Decimal) -> Decimal,
{
    let val_a = resolve_value(a, regs)?;
    let val_b = resolve_value(b, regs)?;
    Some(Instruction::Move(
        dst.clone(),
        Operand::Number(op(val_a, val_b)),
    ))
}

fn try_resolve_branch<'a, F>(
    a: &Operand<'a>,
    b: &Operand<'a>,
    label: &Operand<'a>,
    regs: &[Option<Decimal>; 16],
    check: F,
) -> Option<Instruction<'a>>
where
    F: Fn(Decimal, Decimal) -> bool,
{
    let val_a = resolve_value(a, regs)?;
    let val_b = resolve_value(b, regs)?;
    if check(val_a, val_b) {
        Some(Instruction::Jump(label.clone()))
    } else {
        Some(Instruction::LabelDef("".into())) // NOP
    }
}

fn remove_redundant_moves<'a>(input: Vec<InstructionNode<'a>>) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    for node in input {
        if let Instruction::Move(dst, src) = &node.instruction {
            if dst == src {
                changed = true;
                continue;
            }
        }
        output.push(node);
    }
    (output, changed)
}

fn remove_unreachable_code<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    let mut output = Vec::with_capacity(input.len());
    let mut changed = false;
    let mut dead = false;
    for node in input {
        if let Instruction::LabelDef(_) = node.instruction {
            dead = false;
        }
        if dead {
            changed = true;
            continue;
        }
        match node.instruction {
            Instruction::Jump(_) | Instruction::Jump(Operand::ReturnAddress) => dead = true,
            _ => {}
        }
        output.push(node);
    }
    (output, changed)
}

