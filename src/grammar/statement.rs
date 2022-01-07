use super::{
    lexer::{Operator, TokenKind},
    Parse, ParseRes, Parser,
};
use crate::{
    ast::{Block, Expr, Identifier, Statement},
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
                        "if" => {
                            let offset = parser.current_position();
                            parser.accept_current();
                            let (condition, true_branch, false_branch, len) = if_statement(parser)?;
                            (
                                Statement::IfStatement {
                                    condition,
                                    true_branch: (Box::new(true_branch.0), true_branch.1),
                                    false_branch: false_branch.map(|(a, b)| (Box::new(a), b)),
                                },
                                Span { offset, len },
                            )
                        }
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
                Some(TokenKind::OpenBrace) => {
                    parser.parse().map(|(Block { statements }, block_span)| {
                        (Statement::Block(statements), block_span)
                    })?
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

fn if_statement<'code>(
    parser: &mut Parser<'code>,
) -> ParseRes<(
    (Expr<'code>, Span),
    (Statement<'code>, Span),
    Option<(Statement<'code>, Span)>,
    usize,
)> {
    let (condition, condition_span) =
        parser.with_context("parsing if statement's condition", |parser| {
            parser.expect_token(TokenKind::OpenParen)?;
            parser.accept_current();
            let (condition, span): (_, Span) = parser.parse()?;
            parser.expect_token(TokenKind::CloseParen)?;
            let end = parser.current_position();
            parser.accept_current();
            Ok((
                condition,
                Span {
                    offset: span.offset,
                    len: end - span.offset,
                },
            ))
        })?;

    let (true_branch, true_branch_span): (_, Span) = parser.parse()?;
    let (false_branch, false_branch_span) = match parser.peek_token()? {
        Some(TokenKind::Identifier) if parser.current_token_source() == "else" => {
            let start = parser.current_position();
            parser.accept_current();
            let (stmt, span): (_, Span) = parser.parse()?;

            (
                Some(stmt),
                Span {
                    offset: start,
                    len: span.offset + span.len - start,
                },
            )
        }
        _ => (
            None,
            Span {
                offset: true_branch_span.offset,
                len: 0,
            },
        ),
    };

    let total_span_len = condition_span.len + true_branch_span.len + false_branch_span.len;
    Ok((
        (condition, condition_span),
        (true_branch, true_branch_span),
        false_branch.map(|fb| (fb, false_branch_span)),
        total_span_len,
    ))
}
