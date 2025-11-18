// r0  - r3  : function arguments
// r4  - r9  : temporary variables
// r10 - r15 : persistant variables

use std::collections::HashMap;

enum VarType {
    /// Represents a parameter register (r0 - r3)
    Func(u8),
    /// Represents a temporary register (r4 - r8)
    Temp(u8),
    /// Represents a variable register (r9 - r15)
    Persist(u8),
    /// Represents a stack pointer offset for a given variable
    Stack(u16),
}

#[derive(Default)]
pub struct VariableScope<'a> {
    stack: Vec<HashMap<&'a str, usize>>,
    temp: HashMap<&'a str, u8>,
    persist: HashMap<&'a str, u8>,
    parent: Option<&'a VariableScope<'a>>,
}

impl<'a> VariableScope<'a> {
    pub fn scoped(parent: &'a VariableScope<'a>) -> Self {
        Self {
            parent: Option::Some(parent),
            ..Default::default()
        }
    }
}
