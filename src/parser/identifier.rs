use super::{Parse, ParseRes, Parser};
use crate::ast::Identifier;
use crate::lexer::TokenKind;

impl<'source> Parse<'source> for Identifier<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        parser.with_context("parsing identifier", |parser| {
            parser.expect_token(TokenKind::Identifier)?;
            let src = parser.current_token_source();
            parser.accept_current();
            Ok(Self(src))
        })
    }
}
