use super::{
    lexer::{Operator, TokenKind},
    Parse, ParseRes, Parser,
};
use crate::{
    ast::{Expr, Identifier, Statement},
    error::Span,
};

impl<'source> Parse<'source> for (Statement<'source>, Span) {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        parser.with_context("parsing statement", |parser| {
            Ok(match parser.peek_token()? {
                // TODO: add better description of what is expected
                Some(TokenKind::Identifier) => {
                    let source = parser.current_token_source();
                    let start = parser.current_position();
                    match source {
                        "return" => {
                            parser.accept_current();
                            let return_expr = parser.parse()?;
                            parser.expect_token(TokenKind::Semicolon)?;
                            let end = parser.current_position() + 1;
                            parser.accept_current();
                            (
                                Statement::Return(return_expr),
                                Span {
                                    offset: start,
                                    len: end - start,
                                },
                            )
                        }
                        "int" => {
                            parser.accept_current();
                            let (Identifier(name), span) = parser.parse()?;
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
                            let end = parser.current_position() + 1;
                            parser.accept_current();
                            (
                                Statement::DeclareVar {
                                    name: super::lexer::Source { span, source: name },
                                    init,
                                },
                                Span {
                                    offset: start,
                                    len: end - start,
                                },
                            )
                        }
                        _ => single_expr(parser)?,
                    }
                }
                _ => single_expr(parser)?,
            })
        })
    }
}

fn single_expr<'source>(parser: &mut Parser<'source>) -> ParseRes<(Statement<'source>, Span)> {
    let expr: (Expr, Span) = parser.parse()?;
    parser.expect_token(TokenKind::Semicolon)?;
    parser.accept_current();
    let expr_span = expr.1;
    Ok((Statement::SingleExpr(expr), expr_span))
}
