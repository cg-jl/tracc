use super::lexer::Source;
use super::{lexer::TokenKind, Parse, ParseErrorKind, ParseRes, Parser, WantedSpec};
use crate::ast::Associativity;
use crate::ast::BinaryOp;
use crate::ast::Expr;
use crate::ast::UnaryOp;
use crate::error::Span;

impl<'source> Parse<'source> for (Expr<'source>, Span) {
    fn parse(parser: &mut Parser<'source>) -> ParseRes<Self> {
        parse_primary(parser)
            .and_then(|lhs| {
                parse_binary_expression(parser, lhs, 0)
                    .map_err(|e| e.add_context("parsing binary expression"))
            })
            .map_err(|x| x.add_context("parsing expression"))
    }
}

// parse prefix, simple or parenthesis
fn parse_primary<'source>(parser: &mut Parser<'source>) -> ParseRes<(Expr<'source>, Span)> {
    parser.with_context("parsing primary expression", |parser| {
        // collect all unary operators
        let ops: Vec<_> = {
            let mut vec = Vec::new();
            while let Some(op) = parser
                .peek_token()?
                .and_then(TokenKind::as_operator)
                .and_then(UnaryOp::from_operator)
            {
                vec.push((op, parser.current_token_span()));
                parser.accept_current();
            }
            Ok(vec)
        }?;
        let mut expr = match parser.expect_a_token(Some(WantedSpec::Description("expression")))? {
            TokenKind::OpenParen => {
                let start = parser.current_position();
                parser.accept_current();
                let (e, _) = parser.parse()?;
                parser
                    .expect_token(TokenKind::CloseParen)
                    .map_err(|x| x.add_context("as the end of the expression"))?;
                let end = parser.current_position();
                parser.accept_current();
                Ok((
                    e,
                    Span {
                        offset: start,
                        len: end - start,
                    },
                ))
            }
            TokenKind::Number => {
                let num = parser.current_token_source().parse().unwrap();
                let span = parser.current_token_span();
                parser.accept_current();
                Ok((Expr::Constant(num), span))
            }
            TokenKind::Identifier => {
                let source = parser.current_token_source();
                let span = parser.current_token_span();
                parser.accept_current();
                Ok((
                    Expr::Variable {
                        name: Source { span, source },
                    },
                    span,
                ))
            }
            tok => parser.reject_current_token(ParseErrorKind::Expected {
                found: tok,
                wanted: WantedSpec::Description("open paren, identifier or number"),
            }),
        }?;
        for (operator, Span { offset, len }) in ops.into_iter().rev() {
            expr = (
                Expr::Unary {
                    operator,
                    expr: (Box::new(expr.0), expr.1),
                },
                Span {
                    offset,
                    len: expr.1.offset + expr.1.len + len,
                },
            );
        }
        Ok(expr)
    })
}

fn parse_binary_expression<'source>(
    parser: &mut Parser<'source>,
    mut lhs: (Expr<'source>, Span),
    min_precedence: u8,
) -> ParseRes<(Expr<'source>, Span)> {
    while let Some(op) = parser
        .peek_token()?
        .and_then(TokenKind::as_operator)
        .and_then(BinaryOp::from_operator)
        .filter(|x| x.precedence() >= min_precedence)
    {
        parser.accept_current();
        let mut rhs = parse_primary(parser)?;
        let this_precedence = op.precedence();
        #[allow(clippy::blocks_in_if_conditions)]
        // XXX: I don't know what clippy is trying to tell me; that closure is fine.
        while parser
            .peek_token()?
            .and_then(TokenKind::as_operator)
            .and_then(BinaryOp::from_operator)
            .filter(move |op2| {
                let other_precedence = op2.precedence();
                let assoc = op2.associativity();
                if let Associativity::Left = assoc {
                    other_precedence > this_precedence
                } else {
                    other_precedence == this_precedence
                }
            })
            .is_some()
        {
            rhs = parse_binary_expression(parser, rhs, min_precedence + 1)?;
        }
        lhs = (
            Expr::Binary {
                operator: op,
                lhs: (Box::new(lhs.0), lhs.1),
                rhs: (Box::new(rhs.0), rhs.1),
            },
            Span {
                offset: lhs.1.offset,
                len: rhs.1.offset + rhs.1.len - lhs.1.offset,
            },
        );
    }
    Ok(lhs)
}
