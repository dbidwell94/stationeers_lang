use parser::{
    Parser as ASTParser,
    tree_node::{BlockExpression, Expression, FunctionExpression},
};
use quick_error::quick_error;
use std::{
    collections::HashMap,
    io::{BufWriter, Write},
};

use crate::variable_manager::VariableScope;

quick_error! {
    #[derive(Debug)]
    pub enum CompilerError {
        ParseError(error: parser::ParseError) {
            from()
        }
        IoError(error: std::io::Error) {
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

    pub fn compile(mut self) -> Result<(), CompilerError> {
        let expr = self.parser.parse_all()?;

        let Some(expr) = expr else { return Ok(()) };

        self.write_output("j main")?;
        self.expression(expr, &mut VariableScope::default())
    }

    fn write_output(&mut self, output: impl Into<String>) -> Result<(), CompilerError> {
        self.output.write_all(output.into().as_bytes())?;
        self.output.write_all(b"\n")?;
        self.current_line += 1;
        Ok(())
    }

    fn expression<'v>(
        &mut self,
        expr: Expression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), CompilerError> {
        match expr {
            Expression::Function(expr_func) => self.expression_function(expr_func, scope)?,
            Expression::Block(expr_block) => {
                self.expression_block(expr_block, &mut VariableScope::scoped(&scope))?
            }
            _ => todo!(),
        };

        Ok(())
    }

    fn expression_block<'v>(
        &mut self,
        mut expr: BlockExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), CompilerError> {
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
            if !self.declared_main && !matches!(expr, Expression::Function(_)) {
                self.write_output("main:")?;
                self.declared_main = true;
            }

            self.expression(expr, scope)?;
        }

        Ok(())
    }

    fn expression_function<'v>(
        &mut self,
        expr: FunctionExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), CompilerError> {
        let FunctionExpression {
            name,
            arguments,
            body,
        } = expr;

        if self.function_locations.contains_key(&name) {
            return Err(CompilerError::DuplicateFunction(name));
        }

        self.function_locations
            .insert(name.clone(), self.current_line);

        self.write_output(format!("{}:", name))?;
        self.write_output("push ra")?;

        let mut block_scope = VariableScope::scoped(&scope);

        for (index, var_name) in arguments.iter().enumerate() {
            self.write_output(format!("push r{}", index + 4))?;
            self.write_output(format!(
                "move r{} r{} {}",
                index + 4,
                index,
                if self.config.debug {
                    format!("#{}", var_name)
                } else {
                    "".into()
                }
            ))?;
        }

        self.expression_block(body, &mut block_scope)?;

        for (indx, _) in arguments.iter().enumerate().rev() {
            self.write_output(format!("pop r{}", indx + 4))?;
        }

        self.write_output("pop ra")?;
        self.write_output("j ra")?;
        Ok(())
    }
}
