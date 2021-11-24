use super::{Parse, ParseRes, Parser};
use crate::ast::Block;
use crate::lexer::TokenKind;

// TODO: impl a way to use iterators and flatten them in the final parser. Maybe through a trait?

impl Parse for Block {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        parser.with_context("parsing statement block", |parser| {
            parser.expect_token(TokenKind::OpenBrace)?;
            parser.accept_current();

            let mut statements = Vec::new();

            while parser.peek_token()? != Some(TokenKind::CloseBrace) {
                statements.push(parser.parse()?);
            }

            parser.expect_token(TokenKind::CloseBrace)?;
            parser.accept_current();

            Ok(Self(statements))
        })
    }
}
