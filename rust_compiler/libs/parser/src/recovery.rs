use super::*;

impl<'a> Parser<'a> {
    pub(super) fn synchronize(&mut self) -> Result<(), Error<'a>> {
        self.assign_next()?;

        while let Some(token) = &self.current_token {
            if token.token_type == TokenType::Symbol(Symbol::Semicolon) {
                self.assign_next()?;
                return Ok(());
            }

            match token.token_type {
                TokenType::Keyword(Keyword::Fn)
                | TokenType::Keyword(Keyword::Let)
                | TokenType::Keyword(Keyword::If)
                | TokenType::Keyword(Keyword::While)
                | TokenType::Keyword(Keyword::Loop)
                | TokenType::Keyword(Keyword::Device)
                | TokenType::Keyword(Keyword::Return) => return Ok(()),
                _ => {}
            }

            self.assign_next()?;
        }

        Ok(())
    }

    pub(super) fn assign_next(&mut self) -> Result<(), Error<'a>> {
        if let Some(token) = &self.current_token {
            self.last_token_span = Some(Self::token_to_span(token));
        }

        // Keep reading tokens, caching doc comments and skipping them
        loop {
            self.current_token = self.tokenizer.next_token()?;

            match &self.current_token {
                Some(token) => {
                    if let TokenType::Comment(comment) = &token.token_type {
                        // Cache doc comments for attachment to the next declaration
                        if let tokenizer::token::Comment::Doc(doc_text) = comment {
                            self.cache_doc_comment(doc_text.to_string());
                        }
                        // Skip all comments (both doc and regular)
                        continue;
                    }

                    // If we have a cached doc comment and encounter an identifier, associate them
                    if let TokenType::Identifier(ref id) = token.token_type
                        && let Some(doc) = self.cached_doc_comment.take()
                    {
                        self.store_declaration_doc(id.to_string(), doc);
                    }

                    // Non-comment token, use it as current
                    break;
                }
                None => break,
            }
        }

        Ok(())
    }

    pub(super) fn get_next(&mut self) -> Result<Option<Token<'a>>, Error<'a>> {
        self.assign_next()?;
        Ok(self.current_token.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::{ParseOutput, Parser, parser};
    use indoc::indoc;
    use tokenizer::Tokenizer;

    #[test]
    fn test_block_with_comment_works_as_intended() -> anyhow::Result<()> {
        let mut parser = parser!(indoc! {
            r#"
                loop {
                    let i = 0;

                    // this is a comment.
                    // This would break the closing brace if we didn't handle comments correctly
                }
            "#
        });

        let ParseOutput { root: ast, .. } = parser.parse_all()?.unwrap();

        assert_eq!("{ (loop { (let i = 0); }); }", ast.to_string());

        Ok(())
    }
}
