#![allow(clippy::result_large_err)]
#[cfg(test)]
mod test;

use parser::{Parser, tree_node::*};
use quick_error::quick_error;
use std::io::{BufWriter, Write};

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        ParseError(err: parser::Error) {
            from()
        }
        IoError(err: std::io::Error) {
            from()
        }
    }
}

pub struct FormatterConfig {
    pub indent_spacing: usize,
    pub max_line_width: usize,
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            max_line_width: 90,
            indent_spacing: 2,
        }
    }
}

pub struct Formatter<'a, W: std::io::Write> {
    output: &'a mut BufWriter<W>,
    indent_level: usize,
    parser: Parser<'a>,
    buffer: String,
    config: FormatterConfig,
}

impl<'a, W: std::io::Write> Formatter<'a, W> {
    pub fn new(
        parser: Parser<'a>,
        writer: &'a mut BufWriter<W>,
        config: Option<FormatterConfig>,
    ) -> Self {
        Self {
            output: writer,
            indent_level: 0,
            parser,
            buffer: String::new(),
            config: config.unwrap_or_default(),
        }
    }

    fn pad_start(&mut self) {
        for _ in 0..self.indent_level {
            self.buffer += " ";
        }
    }

    fn up_indent(&mut self) {
        self.indent_level += self.config.indent_spacing;
    }

    fn down_indent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(self.config.indent_spacing);
    }

    fn flush(&mut self) -> Result<(), Error> {
        let to_flush = std::mem::take(&mut self.buffer);
        self.output.write_all(to_flush.as_bytes())?;
        Ok(())
    }

    pub fn format(mut self) -> Result<(), Error> {
        while let Some(expr) = self.parser.parse()? {
            self.expression(expr.node)?;
        }

        if !self.buffer.is_empty() {
            self.flush()?;
        }
        self.output.flush()?;

        Ok(())
    }

    fn expression(&mut self, expr: Expression) -> Result<(), Error> {
        match expr {
            Expression::Assignment(assign_expr) => self.assign_expression(assign_expr.node),
            Expression::ConstDeclaration(const_expr) => self.const_expression(const_expr.node),
            Expression::Literal(lit) => {
                self.literal_expression(lit.node);
                Ok(())
            }
            Expression::Binary(bin_expr) => self.binary_expression(bin_expr.node),

            _ => todo!(),
        }
    }

    fn binary_expression(&mut self, expr: BinaryExpression) -> Result<(), Error> {
        todo!()
    }

    fn literal_expression(&mut self, lit: Literal) {
        let lit_str = match lit {
            Literal::Number(num, og) => og.unwrap_or(num.to_string()),
            Literal::String(str) => format!(r#""{str}""#),
            Literal::Boolean(b) => if b { "true" } else { "false" }.into(),
        };

        self.buffer += &lit_str;
    }

    fn assign_expression(&mut self, expr: AssignmentExpression) -> Result<(), Error> {
        todo!()
    }

    fn const_expression(&mut self, expr: ConstDeclarationExpression) -> Result<(), Error> {
        self.buffer += "const ";
        self.buffer += &expr.name.node;
        self.buffer += " = ";
        let mut temp_buffer = std::mem::take(&mut self.buffer);
        self.literal_expression(expr.value.node);
        if self.buffer.len() + temp_buffer.len() > self.config.max_line_width {
            temp_buffer = std::mem::replace(&mut self.buffer, temp_buffer);
            self.flush()?;
            self.up_indent();
            self.pad_start();
            self.buffer += &temp_buffer;
            self.down_indent();
        } else {
            self.buffer = temp_buffer + &self.buffer;
        }
        self.buffer += ";\n";
        self.flush()?;

        Ok(())
    }
}
