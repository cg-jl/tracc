use super::{Parse, ParseRes, Parser};
use crate::ast::Statement;
use crate::lexer::TokenKind;

impl Parse for Statement {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        parser.with_context("parsing statement", |parser| {
            // currently only return expressions are supported
            parser.keyword("return")?;
            let return_expr = parser.parse()?;
            parser.expect_token(TokenKind::Semicolon)?;
            parser.accept_current();
            Ok(Self::Return(return_expr))
        })
    }
}
