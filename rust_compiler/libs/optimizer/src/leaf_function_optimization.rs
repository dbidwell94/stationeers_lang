use crate::leaf_function::find_leaf_functions;
use il::InstructionNode;

/// Pass: Leaf Function Optimization
/// If a function makes no calls (is a leaf), it doesn't need to save/restore `ra`.
///
/// NOTE: This optimization is DISABLED due to correctness issues.
/// The optimization was designed for a specific calling convention (GET/PUT for RA)
/// but the compiler generates POP ra for return address restoration. Without proper
/// tracking of both conventions and validation of balanced push/pop pairs, this
/// optimization corrupts the stack frame by:
///
/// 1. Removing `push ra` but not `pop ra`, leaving unbalanced push/pop pairs
/// 2. Not accounting for parameter pops that occur before `push sp`
/// 3. Assuming all RA restoration uses GET instruction, but code uses POP
///
/// Example of broken optimization:
/// ```
/// Unoptimized:          Optimized (BROKEN):
/// compare:              pop r8
/// pop r8                pop r9
/// pop r9                ble r9 r8 5
/// push sp               move r10 1
/// push ra               j ra
/// sgt r1 r9 r8          ^ Missing stack frame!
/// ...
/// pop ra
/// pop sp
/// j ra
/// ```
///
/// Future work: Fix by handling both POP and GET calling conventions, validating
/// balanced push/pop pairs, and accounting for parameter pops.
pub fn optimize_leaf_functions<'a>(
    input: Vec<InstructionNode<'a>>,
) -> (Vec<InstructionNode<'a>>, bool) {
    // Optimization disabled - returns input unchanged
    #[allow(unused)]
    let _leaves = find_leaf_functions(&input);
    (input, false)
}
