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
            "set",
            "setBatched",
            "setBatchedNamed",
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
            "s",
            "sb",
            "sbn"
        );
    };
}
