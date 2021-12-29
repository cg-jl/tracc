use super::{
    lexer::{Operator, TokenKind},
    Parse, ParseRes, Parser,
};
use crate::ast::{Identifier, Statement};

impl<'source> Parse<'source> for Statement<'source> {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        parser.with_context("parsing statement", |parser| {
            Ok(match parser.peek_token()? {
                // TODO: add better description of what is expected
                Some(TokenKind::Identifier) => {
                    let source = parser.current_token_source();
                    match source {
                        "return" => {
                            parser.accept_current();
                            let return_expr = parser.parse()?;
                            parser.expect_token(TokenKind::Semicolon)?;
                            parser.accept_current();
                            Self::Return(return_expr)
                        }
                        "int" => {
                            parser.accept_current();
                            let Identifier(name) = parser.parse()?;
                            let init = if let Some(TokenKind::Operator {
                                kind: Operator::Equals,
                                ..
                            }) = parser.peek_token()?
                            {
                                parser.accept_current();
                                parser.parse().map(Some)?
                            } else {
                                None
                            };
                            parser.expect_token(TokenKind::Semicolon)?;
                            parser.accept_current();
                            Self::DeclareVar { name, init }
                        }
                        _ => single_expr(parser)?,
                    }
                }
                _ => single_expr(parser)?,
            })
        })
    }
}

fn single_expr<'source>(parser: &mut Parser<'source>) -> ParseRes<Statement<'source>> {
    let expr = parser.parse()?;
    parser.expect_token(TokenKind::Semicolon)?;
    parser.accept_current();
    Ok(Statement::SingleExpr(expr))
}
