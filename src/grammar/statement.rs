use super::{
    lexer::{Operator, TokenKind},
    Parse, ParseRes, Parser,
};
use crate::{
    ast::{Block, Expr, Identifier, LoopKind, Statement},
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
                        "do" => {
                            let offset = parser.current_position();
                            parser.accept_current();

                            let body: (_, Span) = parser.parse()?;

                            let condition: (_, Span) = {
                                let wanted_spec = crate::error::WantedSpec::Description(
                                    "'while' after the 'do' loop to specify the condition",
                                );

                                let current_tok = parser.peek_token()?.ok_or_else(|| {
                                    parser.error_at_current_token(
                                        super::ParseErrorKind::UnexpectedEOF {
                                            wanted: Some(wanted_spec),
                                        },
                                    )
                                })?;

                                if current_tok != TokenKind::Identifier
                                    || parser.current_token_source() != "while"
                                {
                                    parser.reject_current_token(super::ParseErrorKind::Expected {
                                        wanted: wanted_spec,
                                        found: current_tok,
                                    })
                                } else {
                                    parser.accept_current();
                                    parser.with_context("parsing do-while's condition", |parser| {
                                        parser.expect_token(TokenKind::OpenParen)?;
                                        parser.accept_current();
                                        let condition = parser.parse()?;
                                        parser.expect_token(TokenKind::CloseParen)?;
                                        parser.accept_current();
                                        Ok(condition)
                                    })
                                }
                            }?;

                            let condition_span = condition.1;

                            (
                                Statement::Loop {
                                    condition,
                                    body: (Box::new(body.0), body.1),
                                    kind: LoopKind::DoWhile,
                                },
                                Span {
                                    offset,
                                    len: condition_span.len + condition_span.offset - offset,
                                },
                            )
                        }
                        "for" => {
                            let offset = parser.current_position();
                            parser.accept_current();
                            let ForLoopDecl {
                                begin,
                                condition,
                                body,
                                optional_end: end,
                            } = for_loop(parser)?;
                            let body_span = body.1;

                            (
                                Statement::Block(vec![
                                    begin,
                                    (
                                        Statement::Loop {
                                            condition,
                                            body: (Box::new(body.0), body_span),
                                            kind: if let Some(on_iteration_end) = end {
                                                LoopKind::For {
                                                    on_iteration_end: (
                                                        Box::new(on_iteration_end.0),
                                                        on_iteration_end.1,
                                                    ),
                                                }
                                            } else {
                                                // a for loop without a statement at the end is
                                                // just a scoped while loop.
                                                LoopKind::While
                                            },
                                        },
                                        body_span,
                                    ),
                                ]),
                                Span {
                                    offset,
                                    len: body_span.offset + body_span.len - offset,
                                },
                            )
                        }
                        "break" => {
                            let offset = parser.current_position();
                            parser.accept_current();
                            parser.expect_token(TokenKind::Semicolon)?;
                            parser.accept_current();

                            (Statement::LoopBreak, Span { offset, len: 5 })
                        }
                        "continue" => {
                            let offset = parser.current_position();
                            parser.accept_current();
                            parser.expect_token(TokenKind::Semicolon)?;
                            parser.accept_current();

                            (Statement::LoopContinue, Span { offset, len: 5 })
                        }

                        "while" => {
                            let offset = parser.current_position();
                            parser.accept_current();

                            let (statement, statement_span, condition, condition_span) =
                                while_loop(parser)?;

                            (
                                Statement::Loop {
                                    condition: (condition, condition_span),
                                    body: (Box::new(statement), statement_span),
                                    kind: LoopKind::While,
                                },
                                Span {
                                    offset,
                                    len: statement_span.offset - offset,
                                },
                            )
                        }
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
                        _ => single_expr(parser, true)?,
                    }
                }
                Some(TokenKind::OpenBrace) => {
                    parser.parse().map(|(Block { statements }, block_span)| {
                        (Statement::Block(statements), block_span)
                    })?
                }
                Some(TokenKind::Semicolon) => {
                    let offset = parser.current_position();
                    parser.accept_current();
                    (Statement::Block(Vec::new()), Span { offset, len: 1 })
                }
                _ => single_expr(parser, true)?,
            })
        })
    }
}

fn single_expr<'source>(
    parser: &mut Parser<'source>,
    needs_semi: bool,
) -> ParseRes<(Statement<'source>, Span)> {
    let expr: (Expr, Span) = parser.parse()?;
    if needs_semi {
        parser.expect_token(TokenKind::Semicolon)?;
        parser.accept_current();
    }
    let expr_span = expr.1;
    Ok((Statement::SingleExpr(expr), expr_span))
}

struct ForLoopDecl<'code> {
    begin: (Statement<'code>, Span),
    condition: (Expr<'code>, Span),
    body: (Statement<'code>, Span),
    optional_end: Option<(Statement<'code>, Span)>,
}

fn for_loop<'code>(parser: &mut Parser<'code>) -> ParseRes<ForLoopDecl<'code>> {
    let (begin, condition, optional_end) =
        parser.with_context("parsing for loop header", |parser| {
            parser.expect_token(TokenKind::OpenParen)?;
            parser.accept_current();
            let begin = if parser.peek_token()? == Some(TokenKind::Semicolon) {
                let offset = parser.current_position();
                parser.accept_current();
                (Statement::Block(vec![]), Span { offset, len: 1 })
            } else {
                parser.parse()?
            };

            let cond = if parser.peek_token()? == Some(TokenKind::Semicolon) {
                let offset = parser.current_position();
                parser.accept_current();
                (Expr::Constant(1), Span { offset, len: 1 })
            } else {
                let cond = parser.parse()?;
                parser.expect_token(TokenKind::Semicolon)?;
                parser.accept_current();
                cond
            };
            let end = if parser.peek_token()? == Some(TokenKind::CloseParen) {
                let offset = parser.current_position();
                parser.accept_current();
                None
            } else {
                let expr = single_expr(parser, false)?;

                parser.expect_token(TokenKind::CloseParen)?;
                parser.accept_current();
                Some(expr)
            };
            Ok((begin, cond, end))
        })?;

    let body = parser.parse()?;

    Ok(ForLoopDecl {
        begin,
        condition,
        body,
        optional_end,
    })
}

fn while_loop<'code>(
    parser: &mut Parser<'code>,
) -> ParseRes<(Statement<'code>, Span, Expr<'code>, Span)> {
    let (condition, condition_span) =
        parser.with_context("parsing while loop's condition", |parser| {
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

    let (statement, statement_span) = parser.parse()?;

    Ok((statement, statement_span, condition, condition_span))
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
                offset: true_branch_span.offset + true_branch_span.len,
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
