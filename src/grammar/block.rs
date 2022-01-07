use super::{lexer::TokenKind, Parse, ParseRes, Parser};
use crate::{ast::Block, error::Span};

impl<'source> Parse<'source> for (Block<'source>, Span) {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        parser.with_context("parsing statement block", |parser| {
            parser.expect_token(TokenKind::OpenBrace)?;
            let start = parser.current_position();
            parser.accept_current();

            let mut statements = Vec::new();

            while parser.peek_token()? != Some(TokenKind::CloseBrace) {
                statements.push(parser.parse()?);
            }

            parser.expect_token(TokenKind::CloseBrace)?;
            let end = parser.current_position();
            parser.accept_current();

            Ok((
                Block { statements },
                Span {
                    offset: start,
                    len: end - start,
                },
            ))
        })
    }
}
impl<'source> Parse<'source> for Block<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        parser.with_context("parsing statement block", |parser| {
            parser.expect_token(TokenKind::OpenBrace)?;
            parser.accept_current();

            let mut statements = Vec::new();

            while parser.peek_token()? != Some(TokenKind::CloseBrace) {
                statements.push(parser.parse()?);
            }

            parser.expect_token(TokenKind::CloseBrace)?;
            parser.accept_current();

            Ok(Self { statements })
        })
    }
}
