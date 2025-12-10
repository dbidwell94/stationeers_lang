#[macro_export]
macro_rules! with_syscalls {
    ($matcher:ident) => {
        $matcher!(
            // Big names
            "yield",
            "sleep",
            "hash",
            "load",
            "loadBatched",
            "loadBatchedNamed",
            "loadSlot",
            "set",
            "setBatched",
            "setBatchedNamed",
            "setSlot",
            "acos",
            "asin",
            "atan",
            "atan2",
            "abs",
            "ceil",
            "cos",
            "floor",
            "log",
            "max",
            "min",
            "rand",
            "sin",
            "sqrt",
            "tan",
            "trunc",
            // Lil' names
            "l",
            "lb",
            "lbn",
            "ls",
            "s",
            "sb",
            "sbn",
            "ss"
        );
    };
}
