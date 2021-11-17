use super::{Parse, ParseRes, Parser};
use crate::ast::Block;
use crate::lexer::TokenKind;

impl Parse for Block {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        parser.with_context("parsing statement block", |parser| {
            parser.expect_token(TokenKind::OpenBrace)?;
            parser.accept_current();

            let statements = parser.iterate()?;

            parser.expect_token(TokenKind::CloseBrace)?;
            parser.accept_current();

            Ok(Self(statements))

        })
    }
}
