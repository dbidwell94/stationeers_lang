// r15       : Return Value
// r0        : Unmanaged temp variable
// r1 - r7   : Temporary Variables
// r8 - r14  : Persistant Variables

use lsp_types::{Diagnostic, DiagnosticSeverity};
use parser::tree_node::{Literal, Span};
use std::{
    borrow::Cow,
    collections::{HashMap, VecDeque},
};
use thiserror::Error;

const TEMP: [u8; 7] = [1, 2, 3, 4, 5, 6, 7];
const PERSIST: [u8; 7] = [8, 9, 10, 11, 12, 13, 14];

#[derive(Error, Debug)]
pub enum Error<'a> {
    #[error("{0} already exists.")]
    DuplicateVariable(Cow<'a, str>, Option<Span>),

    #[error("{0} does not exist.")]
    UnknownVariable(Cow<'a, str>, Option<Span>),

    #[error("{0}")]
    Unknown(Cow<'a, str>, Option<Span>),
}

impl<'a> From<Error<'a>> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        match value {
            Error::DuplicateVariable(_, span)
            | Error::UnknownVariable(_, span)
            | Error::Unknown(_, span) => Diagnostic {
                range: span.map(lsp_types::Range::from).unwrap_or_default(),
                severity: Some(DiagnosticSeverity::ERROR),
                message: value.to_string(),
                ..Default::default()
            },
        }
    }
}

/// A request to store a variable at a specific register type
pub enum LocationRequest {
    #[allow(dead_code)]
    /// Request to store a variable in a temprary register.
    Temp,
    /// Request to store a variable in a persistant register.
    Persist,
    /// Request to store a variable in the stack.
    Stack,
}

#[derive(Clone, Debug)]
pub enum VariableLocation<'a> {
    /// Represents a temporary register (r1 - r7)
    Temporary(u8),
    /// Represents a persistant register (r8 - r14)
    Persistant(u8),
    /// Represents a a stack offset (current stack - offset = variable loc)
    Stack(u16),
    /// Represents a constant value and should be directly substituted as such.
    Constant(Literal<'a>),
    /// Represents a device pin. This will contain the exact `d0-d5` string
    Device(Cow<'a, str>),
}

pub struct VariableScope<'a, 'b> {
    temporary_vars: VecDeque<u8>,
    persistant_vars: VecDeque<u8>,
    var_lookup_table: HashMap<Cow<'a, str>, VariableLocation<'a>>,
    stack_offset: u16,
    parent: Option<&'b VariableScope<'a, 'b>>,
}

impl<'a, 'b> Default for VariableScope<'a, 'b> {
    fn default() -> Self {
        Self {
            parent: None,
            stack_offset: 0,
            persistant_vars: PERSIST.to_vec().into(),
            temporary_vars: TEMP.to_vec().into(),
            var_lookup_table: HashMap::new(),
        }
    }
}

impl<'a, 'b> VariableScope<'a, 'b> {
    #[allow(dead_code)]
    pub const TEMP_REGISTER_COUNT: u8 = 7;
    pub const PERSIST_REGISTER_COUNT: u8 = 7;

    pub const RETURN_REGISTER: u8 = 15;
    pub const TEMP_STACK_REGISTER: u8 = 0;

    pub fn registers(&self) -> impl Iterator<Item = &u8> {
        self.var_lookup_table
            .values()
            .filter(|val| {
                matches!(
                    val,
                    VariableLocation::Temporary(_) | VariableLocation::Persistant(_)
                )
            })
            .map(|loc| match loc {
                VariableLocation::Persistant(reg) | VariableLocation::Temporary(reg) => reg,
                _ => unreachable!(),
            })
    }

    pub fn scoped(parent: &'b VariableScope<'a, 'b>) -> Self {
        Self {
            parent: Option::Some(parent),
            temporary_vars: parent.temporary_vars.clone(),
            persistant_vars: parent.persistant_vars.clone(),
            ..Default::default()
        }
    }

    pub fn stack_offset(&self) -> u16 {
        self.stack_offset
    }

    /// Adds and tracks a new scoped variable. If the location you request is full, will fall back
    /// to the stack.
    pub fn add_variable(
        &mut self,
        var_name: Cow<'a, str>,
        location: LocationRequest,
        span: Option<Span>,
    ) -> Result<VariableLocation<'a>, Error<'a>> {
        if self.var_lookup_table.contains_key(&var_name) {
            return Err(Error::DuplicateVariable(var_name, span));
        }
        let var_location = match location {
            LocationRequest::Temp => {
                if let Some(next_var) = self.temporary_vars.pop_front() {
                    VariableLocation::Temporary(next_var)
                } else {
                    let loc = VariableLocation::Stack(self.stack_offset);
                    self.stack_offset += 1;
                    loc
                }
            }
            LocationRequest::Persist => {
                if let Some(next_var) = self.persistant_vars.pop_front() {
                    VariableLocation::Persistant(next_var)
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

    pub fn define_const(
        &mut self,
        var_name: Cow<'a, str>,
        value: Literal<'a>,
        span: Option<Span>,
    ) -> Result<VariableLocation<'a>, Error<'a>> {
        if self.var_lookup_table.contains_key(&var_name) {
            return Err(Error::DuplicateVariable(var_name, span));
        }

        let new_value = VariableLocation::Constant(value);

        self.var_lookup_table.insert(var_name, new_value.clone());
        Ok(new_value)
    }

    pub fn get_location_of(
        &self,
        var_name: &Cow<'a, str>,
        span: Option<Span>,
    ) -> Result<VariableLocation<'a>, Error<'a>> {
        // 1. Check this scope
        if let Some(var) = self.var_lookup_table.get(var_name) {
            if let VariableLocation::Stack(inserted_at_offset) = var {
                // Return offset relative to CURRENT sp
                return Ok(VariableLocation::Stack(
                    self.stack_offset - inserted_at_offset,
                ));
            } else {
                return Ok(var.clone());
            }
        }

        // 2. Recursively check parent
        if let Some(parent) = self.parent {
            let loc = parent.get_location_of(var_name, span)?;

            if let VariableLocation::Stack(parent_offset) = loc {
                return Ok(VariableLocation::Stack(parent_offset + self.stack_offset));
            }
            return Ok(loc);
        }

        Err(Error::UnknownVariable(var_name.clone(), span))
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    #[allow(dead_code)]
    pub fn free_temp(
        &mut self,
        var_name: Cow<'a, str>,
        span: Option<Span>,
    ) -> Result<(), Error<'a>> {
        let Some(location) = self.var_lookup_table.remove(&var_name) else {
            return Err(Error::UnknownVariable(var_name, span));
        };

        match location {
            VariableLocation::Temporary(t) => {
                self.temporary_vars.push_back(t);
            }
            VariableLocation::Persistant(_) => {
                return Err(Error::UnknownVariable(
                    Cow::from("Attempted to free a `let` variable."),
                    span,
                ));
            }
            _ => {}
        };

        Ok(())
    }
}
