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

                if parser.peek_token()? == Some(TokenKind::OpenParen) {
                    let offset = span.offset;
                    parser.accept_current();

                    let mut args = Vec::new();

                    while parser
                        .expect_a_token(Some(WantedSpec::Description("argument or close paren")))?
                        != TokenKind::CloseParen
                    {
                        args.push(parser.parse()?);
                        if parser
                            .expect_a_token(Some(WantedSpec::Description("comma or close paren")))?
                            == TokenKind::Comma
                        {
                            parser.accept_current();
                        } else {
                            break;
                        }
                    }

                    parser.expect_token(TokenKind::CloseParen)?;
                    let end = parser.current_position();
                    parser.accept_current();
                    Ok((
                        Expr::Call {
                            name: (source, span),
                            args,
                        },
                        Span {
                            offset,
                            len: end - offset,
                        },
                    ))
                } else {
                    Ok((
                        Expr::Variable {
                            name: Source { span, source },
                        },
                        span,
                    ))
                }
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

// Use a mid step to detect ternary operator
#[derive(Clone, Copy, Debug)]
enum DetectTernary {
    NormalOp(BinaryOp),
    Ternary,
}

impl DetectTernary {
    const fn associativity(self) -> Associativity {
        match self {
            Self::NormalOp(op) => op.associativity(),
            Self::Ternary => Associativity::RightToLeft,
        }
    }
    fn from_operator((op, has_equal): (super::lexer::Operator, bool)) -> Option<Self> {
        use super::lexer::Operator::Ternary;
        if let Ternary = op {
            Some(Self::Ternary)
        } else {
            BinaryOp::from_operator((op, has_equal)).map(Self::NormalOp)
        }
    }

    const fn precedence(&self) -> u8 {
        match self {
            Self::NormalOp(op) => op.precedence(),
            Self::Ternary => 15 - 13,
        }
    }

    // consume more tokens if needed since we accepted the operator
    fn builder<'code>(self, parser: &mut Parser<'code>) -> ParseRes<BinExprBuilder<'code>> {
        parser.accept_current();
        match self {
            Self::NormalOp(op) => Ok(BinExprBuilder::NormalOp(op)),
            Self::Ternary => {
                let middle = parser.parse()?;
                parser.expect_token(TokenKind::Colon).map_err(|err| {
                    err.add_context("`?` from ternaary operator needs a `:` for the `else` part")
                })?;
                parser.accept_current();
                Ok(BinExprBuilder::Ternary { middle })
            }
        }
    }
}

enum BinExprBuilder<'code> {
    NormalOp(BinaryOp),
    Ternary { middle: (Expr<'code>, Span) },
}

impl<'code> BinExprBuilder<'code> {
    fn build(
        self,
        (lhs_expr, lhs_span): (Expr<'code>, Span),
        (rhs_expr, rhs_span): (Expr<'code>, Span),
    ) -> Expr<'code> {
        match self {
            Self::NormalOp(operator) => Expr::Binary {
                operator,
                lhs: (Box::new(lhs_expr), lhs_span),
                rhs: (Box::new(rhs_expr), rhs_span),
            },
            Self::Ternary {
                middle: (true_expr, true_span),
            } => Expr::Ternary {
                condition: (Box::new(lhs_expr), lhs_span),
                value_true: (Box::new(true_expr), true_span),
                value_false: (Box::new(rhs_expr), rhs_span),
            },
        }
    }
}
//
//     fn precedence(&self) -> u8 {
//         match self {
//             Self::NormalOp(op) => op.precedence(),
//             Self::Ternary { .. } => 15 - 13,
//         }
//     }
// }

// fn next_binary_op<'code>(parser: &mut Parser<'code>) -> ParseRes<Option<PreBinaryOp<'code>>> {
//     use super::lexer::Operator::Ternary;
//     if let Some(op) = parser.peek_token()?.and_then(TokenKind::as_operator) {
//         if let Ternary = op.0 {
//             parser.accept_current();
//             let middle = parser.parse()?;
//             parser
//                 .expect_token(TokenKind::Colon)
//                 .map_err(|e| e.add_context("parsing ternary expression"))?;
//             parser.accept_current();
//             Ok(Some(PreBinaryOp::Ternary { middle }))
//         } else {
//             BinaryOp::from_operator(op)
//                 .map(|op| {
//                     parser.accept_current();
//                     Ok(PreBinaryOp::NormalOp(op))
//                 })
//                 .transpose()
//         }
//     } else {
//         Ok(None)
//     }
// }

fn parse_binary_expression<'source>(
    parser: &mut Parser<'source>,
    mut lhs: (Expr<'source>, Span),
    min_precedence: u8,
) -> ParseRes<(Expr<'source>, Span)> {
    while let Some(op) = parser
        .peek_token()?
        .and_then(TokenKind::as_operator)
        .and_then(DetectTernary::from_operator)
        .filter(|x| x.precedence() >= min_precedence)
    {
        let this_precedence = op.precedence();
        let builder = op.builder(parser)?;
        let mut rhs = parse_primary(parser)?;
        #[allow(clippy::blocks_in_if_conditions)]
        // XXX: I don't know what clippy is trying to tell me; that closure is fine.
        while parser
            .peek_token()?
            .and_then(TokenKind::as_operator)
            .and_then(DetectTernary::from_operator)
            .filter(move |op2| {
                let other_precedence = op2.precedence();
                match op2.associativity() {
                    Associativity::RightToLeft => other_precedence >= this_precedence,
                    Associativity::LeftToRight => other_precedence > this_precedence,
                }
            })
            .is_some()
        {
            rhs = parse_binary_expression(parser, rhs, min_precedence + 1)?;
        }
        let span = Span {
            offset: lhs.1.offset,
            len: rhs.1.offset + rhs.1.len - lhs.1.offset,
        };
        lhs = (builder.build(lhs, rhs), span);
    }
    Ok(lhs)
}
