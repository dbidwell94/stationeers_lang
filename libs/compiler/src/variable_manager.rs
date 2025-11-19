// r0  - r7  : temporary variables
// r8 - r15 : persistant variables

use quick_error::quick_error;
use std::collections::{HashMap, VecDeque};

const TEMP: [u8; 8] = [0, 1, 2, 3, 4, 5, 6, 7];
const PERSIST: [u8; 8] = [8, 9, 10, 11, 12, 13, 14, 15];

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        DuplicateVariable(var: String) {
            display("{var} already exists.")
        }
        UnknownVariable(var: String) {
            display("{var} does not exist.")
        }
        Unknown(reason: String) {
            display("{reason}")
        }
    }
}

pub enum VarType {
    /// Represents a temporary register (r4 - r8)
    Temp(u8),
    /// Represents a variable register (r9 - r15)
    Persist(u8),
    /// Represents a stack pointer offset for a given variable
    Stack(u16),
}

/// A request to store a variable at a specific register type
pub enum LocationRequest {
    /// Request to store a variable in a temprary register.
    Temp,
    /// Request to store a variable in a persistant register.
    Persist,
    /// Request to store a variable in the stack.
    Stack,
}

#[derive(Clone)]
pub enum VariableLocation {
    Temporary(u8),
    Persistant(u8),
    Stack(u16),
}

pub struct VariableScope<'a> {
    temporary_vars: VecDeque<u8>,
    persistant_vars: VecDeque<u8>,
    var_lookup_table: HashMap<String, VariableLocation>,
    var_stack: HashMap<String, u16>,
    stack_offset: u16,
    parent: Option<&'a VariableScope<'a>>,
}

impl<'a> Default for VariableScope<'a> {
    fn default() -> Self {
        Self {
            parent: None,
            stack_offset: 0,
            persistant_vars: PERSIST.to_vec().into(),
            temporary_vars: TEMP.to_vec().into(),
            var_stack: HashMap::new(),
            var_lookup_table: HashMap::new(),
        }
    }
}

impl<'a> VariableScope<'a> {
    pub const TEMP_REGISTER_COUNT: u8 = 8;
    pub const PERSIST_REGISTER_COUNT: u8 = 8;

    pub fn scoped(parent: &'a VariableScope<'a>) -> Self {
        Self {
            parent: Option::Some(parent),
            ..Default::default()
        }
    }

    pub fn stack_offset(&self) -> u16 {
        self.stack_offset
    }

    pub fn stack_incr(&mut self) {
        self.stack_offset += 1;
    }

    /// Adds and tracks a new scoped variable. If the location you request is full, will fall back
    /// to the stack.
    pub fn add_variable(
        &mut self,
        var_name: impl Into<String>,
        location: LocationRequest,
    ) -> Result<VariableLocation, Error> {
        let var_name = var_name.into();
        if self.var_lookup_table.contains_key(var_name.as_str()) {
            return Err(Error::DuplicateVariable(var_name));
        }
        let var_location = match location {
            LocationRequest::Temp => {
                if let Some(next_var) = self.temporary_vars.pop_front() {
                    let loc = VariableLocation::Temporary(next_var);
                    loc
                } else {
                    let loc = VariableLocation::Stack(self.stack_offset);
                    self.stack_offset += 1;
                    loc
                }
            }
            LocationRequest::Persist => {
                if let Some(next_var) = self.persistant_vars.pop_front() {
                    let loc = VariableLocation::Persistant(next_var);
                    loc
                } else {
                    let loc = VariableLocation::Stack(self.stack_offset);
                    self.stack_offset += 1;
                    loc
                }
            }
            LocationRequest::Stack => {
                let loc = VariableLocation::Stack(self.stack_offset);
                self.stack_offset += 1;
                loc
            }
        };
        self.var_lookup_table.insert(var_name, var_location.clone());

        Ok(var_location)
    }

    pub fn get_location_of(
        &mut self,
        var_name: impl Into<String>,
    ) -> Result<VariableLocation, Error> {
        let var_name = var_name.into();
        self.var_lookup_table
            .get(var_name.as_str())
            .map(|v| v.clone())
            .ok_or(Error::UnknownVariable(var_name))
    }

    pub fn free_temp(&mut self, var_name: impl Into<String>) -> Result<(), Error> {
        let var_name = var_name.into();
        let Some(location) = self.var_lookup_table.remove(var_name.as_str()) else {
            return Err(Error::UnknownVariable(var_name));
        };

        match location {
            VariableLocation::Temporary(t) => {
                self.temporary_vars.push_back(t);
            }
            VariableLocation::Persistant(_) => {
                return Err(Error::UnknownVariable(String::from(
                    "Attempted to free a `let` variable.",
                )));
            }
            VariableLocation::Stack(_) => {}
        };

        Ok(())
    }
}
