use super::{Parse, ParseRes, Parser};
use crate::ast::Identifier;
use crate::lexer::TokenKind;

impl Parse for Identifier {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        parser.with_context("parsing identifier", |parser| {
            parser.expect_token(TokenKind::Identifier)?;
            let src = parser.current_token_source();
            parser.accept_current();
            Ok(Self(src.to_string()))
        })
    }
}
