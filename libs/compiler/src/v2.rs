use crate::variable_manager::{self, LocationRequest, VariableLocation, VariableScope};
use parser::{
    Parser as ASTParser,
    tree_node::{BlockExpression, Expression, FunctionExpression},
};
use quick_error::quick_error;
use std::{
    collections::HashMap,
    io::{BufWriter, Write},
};

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        ParseError(error: parser::Error) {
            from()
        }
        IoError(error: std::io::Error) {
            from()
        }
        ScopeError(error: variable_manager::Error) {
            from()
        }
        DuplicateFunction(func_name: String) {
            display("{func_name} has already been defined")
        }
    }
}

#[derive(Default)]
pub struct CompilerConfig {
    pub debug: bool,
}

pub struct Compiler<'a, W: std::io::Write> {
    parser: ASTParser,
    /// Max stack size for the program is by default 512.
    variable_scope: Vec<HashMap<String, i32>>,
    function_locations: HashMap<String, usize>,
    devices: HashMap<String, String>,
    output: &'a mut BufWriter<W>,
    current_line: usize,
    declared_main: bool,
    config: CompilerConfig,
}

impl<'a, W: std::io::Write> Compiler<'a, W> {
    pub fn new(
        parser: ASTParser,
        writer: &'a mut BufWriter<W>,
        config: Option<CompilerConfig>,
    ) -> Self {
        Self {
            parser,
            variable_scope: Vec::new(),
            function_locations: HashMap::new(),
            devices: HashMap::new(),
            output: writer,
            current_line: 1,
            declared_main: false,
            config: config.unwrap_or_default(),
        }
    }

    pub fn compile(mut self) -> Result<(), Error> {
        let expr = self.parser.parse_all()?;

        let Some(expr) = expr else { return Ok(()) };

        self.write_output("j main")?;
        self.expression(expr, &mut VariableScope::default())
    }

    fn write_output(&mut self, output: impl Into<String>) -> Result<(), Error> {
        self.output.write_all(output.into().as_bytes())?;
        self.output.write_all(b"\n")?;
        self.current_line += 1;
        Ok(())
    }

    fn expression<'v>(
        &mut self,
        expr: Expression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        match expr {
            Expression::Function(expr_func) => self.expression_function(expr_func, scope)?,
            Expression::Block(expr_block) => self.expression_block(expr_block, scope)?,
            _ => todo!(),
        };

        Ok(())
    }

    fn expression_block<'v>(
        &mut self,
        mut expr: BlockExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        // First, sort the expressions to ensure functions are hoisted
        expr.0.sort_by(|a, b| {
            if matches!(b, Expression::Function(_)) && matches!(a, Expression::Function(_)) {
                std::cmp::Ordering::Equal
            } else if matches!(a, Expression::Function(_)) {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Greater
            }
        });

        for expr in expr.0 {
            if !self.declared_main
                && !matches!(expr, Expression::Function(_))
                && !scope.has_parent()
            {
                self.write_output("main:")?;
                self.declared_main = true;
            }

            self.expression(expr, scope)?;
        }

        Ok(())
    }

    /// Compile a function declaration.
    /// Calees are responsible for backing up any registers they wish to use.
    fn expression_function<'v>(
        &mut self,
        expr: FunctionExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        let FunctionExpression {
            name,
            arguments,
            body,
        } = expr;

        if self.function_locations.contains_key(&name) {
            return Err(Error::DuplicateFunction(name));
        }

        self.function_locations
            .insert(name.clone(), self.current_line);

        // Declare the function as a line identifier
        self.write_output(format!("{}:", name))?;

        // Create a new block scope for the function body
        let mut block_scope = VariableScope::scoped(&scope);

        let mut saved_variables = 0;

        // do a reverse pass to pop variables from the stack and put them into registers
        for var_name in arguments
            .iter()
            .rev()
            .take(VariableScope::PERSIST_REGISTER_COUNT as usize)
        {
            let loc = block_scope.add_variable(var_name, LocationRequest::Persist)?;
            // we don't need to imcrement the stack offset as it's already on the stack from the
            // previous scope

            match loc {
                VariableLocation::Persistant(loc) => {
                    self.write_output(format!(
                        "pop r{loc} {}",
                        if self.config.debug {
                            format!("#{}", var_name)
                        } else {
                            "".into()
                        }
                    ))?;
                }
                VariableLocation::Stack(_) => {
                    unimplemented!("Attempted to save to stack without tracking in scope")
                }

                _ => {
                    unimplemented!(
                        "Attempted to return a Temporary scoped variable from a Persistant request"
                    )
                }
            }
            saved_variables += 1;
        }

        // now do a forward pass in case we have spilled into the stack. We don't need to push
        // anything as they already exist on the stack, but we DO need to let our block_scope be
        // aware that the variables exist on the stack (left to right)
        for var_name in arguments.iter().take(arguments.len() - saved_variables) {
            block_scope.add_variable(var_name, LocationRequest::Stack)?;
        }

        self.write_output("push ra")?;
        block_scope.add_variable(format!("{name}_ra"), LocationRequest::Stack)?;

        self.expression_block(body, &mut block_scope)?;
        // Get the saved return address and save it back into `ra`
        let VariableLocation::Stack(ra_stack_offset) =
            block_scope.get_location_of(format!("{name}_ra"))?
        else {
            panic!("This shouldn't happen");
        };

        self.write_output(format!("sub r0 sp {ra_stack_offset}"))?;
        self.write_output("get ra db r0")?;

        if block_scope.stack_offset() > 0 {
            self.write_output(format!("sub sp sp {}", block_scope.stack_offset()))?;
        }

        self.write_output("j ra")?;
        Ok(())
    }
}
