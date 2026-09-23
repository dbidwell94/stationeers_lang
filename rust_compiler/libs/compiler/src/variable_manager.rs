// r15       : Return Value
// r0        : Unmanaged temp variable
// r1 - r7   : Temporary Variables
// r8 - r14  : Persistant Variables

use helpers::Span;
use lsp_types::{Diagnostic, DiagnosticSeverity};
use parser::tree_node::{DeviceType, Literal};
use std::{
    borrow::Cow,
    collections::{HashMap, VecDeque},
};
use thiserror::Error;

const TEMP: [u8; 7] = [1, 2, 3, 4, 5, 6, 7];
const PERSIST: [u8; 7] = [8, 9, 10, 11, 12, 13, 14];

/// Number of `db` stack slots (out of 512 total) reserved for user arrays.
/// The remaining slots are reserved for the compiler's own variable spilling.
pub const ARRAY_REGION_SIZE: u16 = 256;

#[derive(Error, Debug)]
pub enum Error<'a> {
    #[error("{0} already exists.")]
    DuplicateVariable(Cow<'a, str>, Option<Span>),

    #[error("{0} does not exist.")]
    UnknownVariable(Cow<'a, str>, Option<Span>),

    #[error("{0}")]
    Unknown(Cow<'a, str>, Option<Span>),

    #[error("Array storage exceeded: {0} slots requested, only {1} available.")]
    ArrayCapacityExceeded(u16, u16, Option<Span>),
}

impl<'a> Error<'a> {
    pub fn into_owned(self) -> Error<'static> {
        match self {
            Error::DuplicateVariable(name, span) => {
                Error::DuplicateVariable(Cow::Owned(name.into_owned()), span)
            }
            Error::UnknownVariable(name, span) => {
                Error::UnknownVariable(Cow::Owned(name.into_owned()), span)
            }
            Error::Unknown(message, span) => Error::Unknown(Cow::Owned(message.into_owned()), span),
            Error::ArrayCapacityExceeded(requested, available, span) => {
                Error::ArrayCapacityExceeded(requested, available, span)
            }
        }
    }
}

impl<'a> From<Error<'a>> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        match value {
            Error::DuplicateVariable(_, span)
            | Error::UnknownVariable(_, span)
            | Error::Unknown(_, span)
            | Error::ArrayCapacityExceeded(_, _, span) => Diagnostic {
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
    Device(DeviceType),
    /// Represents a fixed, absolute `db` stack address range reserved for a
    /// user array: `base` is the starting address, `len` the element count.
    Array { base: u16, len: u16 },
}

pub struct VariableScope<'a, 'b> {
    temporary_vars: VecDeque<u8>,
    persistant_vars: VecDeque<u8>,
    var_lookup_table: HashMap<Cow<'a, str>, VariableLocation<'a>>,
    device_reference_lookup_table: HashMap<Cow<'a, str>, DeviceType>,
    stack_offset: u16,
    /// Number of array slots allocated in this scope (absolute addresses,
    /// not relative to `sp`). Freed automatically when the scope is dropped,
    /// since sibling scopes are constructed fresh with `array_offset: 0`.
    array_offset: u16,
    parent: Option<&'b VariableScope<'a, 'b>>,
}

impl<'a, 'b> Default for VariableScope<'a, 'b> {
    fn default() -> Self {
        Self {
            parent: None,
            stack_offset: 0,
            array_offset: 0,
            persistant_vars: PERSIST.to_vec().into(),
            temporary_vars: TEMP.to_vec().into(),
            var_lookup_table: HashMap::new(),
            device_reference_lookup_table: HashMap::new(),
        }
    }
}

impl<'a, 'b> VariableScope<'a, 'b> {
    #[allow(dead_code)]
    pub const TEMP_REGISTER_COUNT: u8 = 7;
    pub const PERSIST_REGISTER_COUNT: u8 = 7;

    pub const RETURN_REGISTER: u8 = 15;
    pub const TEMP_STACK_REGISTER: u8 = 0;

    pub fn registers(&self) -> Vec<u8> {
        let mut used = Vec::new();

        for r in TEMP {
            if !self.temporary_vars.contains(&r) {
                used.push(r);
            }
        }

        for r in PERSIST {
            if !self.persistant_vars.contains(&r) {
                used.push(r);
            }
        }
        used
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

    /// Returns the total stack offset including all parent scopes.
    /// This is useful for calculating stack cleanup when jumping out of nested scopes.
    pub fn total_stack_depth(&self) -> u16 {
        let mut total = self.stack_offset;
        if let Some(parent) = self.parent {
            total += parent.total_stack_depth();
        }
        total
    }

    /// Returns the total number of array slots consumed by this scope and all
    /// of its ancestors. Used to compute the next array's absolute base address.
    pub fn total_array_depth(&self) -> u16 {
        let mut total = self.array_offset;
        if let Some(parent) = self.parent {
            total += parent.total_array_depth();
        }
        total
    }

    /// Allocates a fixed-size array at the next available absolute `db` stack
    /// address. Arrays are freed implicitly when their declaring scope is
    /// dropped, since sibling scopes start with a fresh `array_offset` of 0
    /// and therefore reuse the same absolute addresses.
    pub fn define_array(
        &mut self,
        var_name: Cow<'a, str>,
        len: u16,
        span: Option<Span>,
    ) -> Result<VariableLocation<'a>, Error<'a>> {
        if self.var_lookup_table.contains_key(&var_name) {
            return Err(Error::DuplicateVariable(var_name, span));
        }

        let parent_depth = self.parent.map(|p| p.total_array_depth()).unwrap_or(0);
        let base = parent_depth + self.array_offset;

        if base + len > ARRAY_REGION_SIZE {
            return Err(Error::ArrayCapacityExceeded(base + len, ARRAY_REGION_SIZE, span));
        }

        self.array_offset += len;

        let new_value = VariableLocation::Array { base, len };
        self.var_lookup_table.insert(var_name, new_value.clone());
        Ok(new_value)
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

    /// Adds and tracks a new constant variable. This is used to track literal values that are
    /// used in the code. These are not stored in registers, but are instead substituted directly
    /// into the code.
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

    /// Defines a device variable. This is used to track device pins, references, etc.
    /// Device _channels_ are not implemented yet, but those will also be tracked here.
    pub fn define_device(
        &mut self,
        var_name: Cow<'a, str>,
        device: DeviceType,
        span: Option<Span>,
    ) -> Result<VariableLocation<'a>, Error<'a>> {
        if self.var_lookup_table.contains_key(&var_name) {
            return Err(Error::DuplicateVariable(var_name, span));
        }

        let new_value = VariableLocation::Device(device);

        self.var_lookup_table.insert(var_name, new_value.clone());
        Ok(new_value)
    }

    pub fn define_device_reference(&mut self, var_name: Cow<'a, str>, device: DeviceType) {
        self.device_reference_lookup_table.insert(var_name, device);
    }

    pub fn get_device_reference(&self, var_name: &Cow<'a, str>) -> Option<DeviceType> {
        self.device_reference_lookup_table
            .get(var_name)
            .cloned()
            .or_else(|| {
                self.parent
                    .and_then(|parent| parent.get_device_reference(var_name))
            })
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
