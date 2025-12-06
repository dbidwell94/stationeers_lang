mod const_expr;

#[macro_export]
macro_rules! fix_format {
    ($item:expr) => {{
        let mut writer = std::io::BufWriter::new(vec![]);
        let tokenizer = tokenizer::Tokenizer::from($item);
        let parser = parser::Parser::new(tokenizer);

        let formatter = crate::Formatter::new(parser, &mut writer, None);

        formatter.format()?;
        String::from_utf8(writer.into_inner()?)?
    }};
}
