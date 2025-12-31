use il::Instructions;

// Optimization pass modules
mod helpers;
mod leaf_function;

mod algebraic_simplification;
mod constant_propagation;
mod dead_code;
mod dead_store_elimination;
mod function_call_optimization;
mod label_resolution;
mod leaf_function_optimization;
mod peephole_optimization;
mod register_forwarding;
mod strength_reduction;

use algebraic_simplification::algebraic_simplification;
use constant_propagation::constant_propagation;
use dead_code::{remove_redundant_jumps, remove_redundant_moves, remove_unreachable_code};
use dead_store_elimination::dead_store_elimination;
use function_call_optimization::optimize_function_calls;
use label_resolution::resolve_labels;
use leaf_function_optimization::optimize_leaf_functions;
use peephole_optimization::peephole_optimization;
use register_forwarding::register_forwarding;
use strength_reduction::strength_reduction;

/// Entry point for the optimizer.
pub fn optimize<'a>(instructions: Instructions<'a>) -> Instructions<'a> {
    let mut instructions = instructions.into_inner();
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

        // Pass 5: Algebraic Simplification (Identity operations)
        let (new_inst, c5) = algebraic_simplification(instructions);
        instructions = new_inst;
        changed |= c5;

        // Pass 6: Strength Reduction (Replace expensive ops with cheaper ones)
        let (new_inst, c6) = strength_reduction(instructions);
        instructions = new_inst;
        changed |= c6;

        // Pass 7: Peephole Optimizations (Common patterns)
        let (new_inst, c7) = peephole_optimization(instructions);
        instructions = new_inst;
        changed |= c7;

        // Pass 8: Dead Store Elimination
        let (new_inst, c8) = dead_store_elimination(instructions);
        instructions = new_inst;
        changed |= c8;

        // Pass 9: Redundant Move Elimination
        let (new_inst, c9) = remove_redundant_moves(instructions);
        instructions = new_inst;
        changed |= c9;

        // Pass 10: Dead Code Elimination
        let (new_inst, c10) = remove_unreachable_code(instructions);
        instructions = new_inst;
        changed |= c10;
    }

    // Final Pass: Resolve Labels to Line Numbers
    let mut instructions = resolve_labels(instructions);

    // Post-resolution Pass: Remove redundant jumps (must run after label resolution)
    let (instructions, _) = remove_redundant_jumps(instructions);

    Instructions::new(instructions)
}
