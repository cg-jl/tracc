use super::{lexer::TokenKind, Parse, ParseRes, Parser};
use crate::{ast::Identifier, error::Span};

impl<'source> Parse<'source> for (Identifier<'source>, Span) {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        parser.with_context("parsing identifier", |parser| {
            parser.expect_token(TokenKind::Identifier)?;
            let src = parser.current_token_source();
            let span = parser.current_token_span();
            parser.accept_current();
            Ok((Identifier(src), span))
        })
    }
}
