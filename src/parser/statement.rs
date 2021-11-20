use super::{Parse, ParseRes, Parser};
use crate::ast::Identifier;
use crate::ast::Statement;
use crate::lexer::Operator;
use crate::lexer::TokenKind;

// TODO: parse an optional expression to declare variable

impl Parse for Statement {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
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

fn single_expr(parser: &mut Parser) -> ParseRes<Statement> {
    let expr = parser.parse()?;
    parser.expect_token(TokenKind::Semicolon)?;
    parser.accept_current();
    Ok(Statement::SingleExpr(expr))
}
