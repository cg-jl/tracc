use super::{lexer::TokenKind, Parse, ParseRes, Parser};
use crate::ast::Function;

impl<'source> Parse<'source> for Function<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        parser.with_context("parsing function", |parser| {
            parser.keyword("int")?;
            let (name, _) = parser.parse()?;
            parser.expect_token(TokenKind::OpenParen)?;
            parser.accept_current();

            let mut args = Vec::new();
            while parser.expect_a_token(Some(crate::error::WantedSpec::Description(
                "argument or ')' to close the parameter list",
            )))? != TokenKind::CloseParen
            {
                parser.expect_token(TokenKind::Identifier)?;
                parser.accept_current();

                args.push(parser.parse()?);
                if parser
                    .expect_a_token(Some(crate::error::WantedSpec::Description("comma or ')'")))?
                    != TokenKind::Comma
                {
                    break;
                } else {
                    parser.accept_current();
                }
            }

            parser.expect_token(TokenKind::CloseParen)?;
            parser.accept_current();

            let body = parser.parse()?;

            Ok(Self { name, body, args })

            // parser.expect_token(TokenKind::OpenBrace)?;
            // parser.accept_current();

            // let body = parser.iterate().collect();

            // // parser.keyword("return")?;
            // // let return_expr = parser.parse()?;
            // // parser.expect_token(TokenKind::Semicolon)?;
            // parser.accept_current();
            // parser.expect_token(TokenKind::CloseBrace)?;
            // parser.accept_current();
            // Ok(Self { name, body })
        })
    }
}
