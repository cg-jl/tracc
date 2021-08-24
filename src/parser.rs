use crate::ast::*;
use crate::error::*;
use crate::lexer::*;

// TODO: add measureme to the parser

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_tok: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a SourceMetadata<'a>) -> Self {
        Self {
            lexer: Lexer::new(source),
            current_tok: None,
        }
    }
    pub fn peek_token(&mut self) -> ParseRes<Option<TokenKind>> {
        if self.current_tok.is_none() {
            let next_tok_opt = self
                .lexer
                .next_token()
                .map_err(|e| e.map_kind(ParseErrorKind::LexError))?;
            self.current_tok = next_tok_opt;
        }
        Ok(self.current_tok.as_ref().map(|x| x.kind))
    }
    pub fn current_token_span(&self) -> Span {
        self.current_tok
            .as_ref()
            .map(|x| x.source.span)
            .expect("called current_token_span with no token")
    }
    pub fn current_token_source(&self) -> &'a str {
        self.current_tok
            .as_ref()
            .map(|x| x.source.source)
            .expect("called current_token_source with no token")
    }
    pub fn accept_current(&mut self) {
        self.current_tok = None;
    }
    pub fn emit_error_at<T>(&self, span: Span, kind: ParseErrorKind) -> ParseRes<T> {
        Err(ParseError::new(kind).with_source(span, self.lexer.get_metadata()))
    }
    pub fn expect_a_token(&mut self, wanted: Option<WantedSpec<TokenKind>>) -> ParseRes<TokenKind> {
        let span = self.lexer.current_span();
        self.peek_token()?.map_or_else(
            || self.emit_error_at(span, ParseErrorKind::UnexpectedEOF { wanted }),
            Ok,
        )
    }
    pub fn reject_current_token<T>(&self, reason: ParseErrorKind) -> ParseRes<T> {
        let span = self.current_token_span();
        self.emit_error_at(span, reason)
    }
    pub fn expect_token(&mut self, kind: TokenKind) -> ParseRes<()> {
        self.expect_a_token(Some(WantedSpec::Specific(kind)))
            .and_then(|tok| {
                if tok != kind {
                    self.reject_current_token(ParseErrorKind::Expected {
                        wanted: WantedSpec::Specific(kind),
                        found: tok,
                    })
                } else {
                    Ok(())
                }
            })
    }
    pub fn keyword(&mut self, kw: &'static str) -> ParseRes<()> {
        self.expect_token(TokenKind::Identifier)
            .map_err(|e| e.add_context("parsing keyword"))?;
        let src = self.current_token_source();
        if src != kw {
            self.reject_current_token(ParseErrorKind::Expected {
                wanted: WantedSpec::Description(kw),
                found: TokenKind::Identifier,
            })
        } else {
            self.accept_current();
            Ok(())
        }
    }
    pub fn parse<T>(&mut self) -> ParseRes<T>
    where
        T: Parse,
    {
        T::parse(self)
    }
    pub fn with_context<F, T>(&mut self, context: &'static str, mut cont: F) -> ParseRes<T>
    where
        F: FnMut(&mut Self) -> ParseRes<T>,
    {
        cont(self).map_err(|x| x.add_context(context))
    }
}

pub type ParseRes<T> = Result<T, ParseError>;
pub type ParseError = Error<ParseErrorKind>;

#[derive(Debug)]
pub enum ParseErrorKind {
    LexError(LexErrorKind),
    Expected {
        wanted: WantedSpec<TokenKind>,
        found: TokenKind,
    },
    UnexpectedEOF {
        wanted: Option<WantedSpec<TokenKind>>,
    },
}

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> ParseRes<Self>;
}

use std::error;
impl error::Error for ParseErrorKind {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        if let Self::LexError(err) = self {
            Some(err)
        } else {
            None
        }
    }
}

use std::fmt;
impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::LexError(err) => write!(f, "error while lexing source: {}", err),
            Self::UnexpectedEOF { wanted } => {
                write!(f, "unexpected end of input")?;
                if let Some(wanted) = wanted {
                    write!(f, ", expected {}", wanted)
                } else {
                    Ok(())
                }
            }
            Self::Expected { wanted, found } => {
                write!(f, "expected {}, but found instead {}", wanted, found)
            }
        }
    }
}

impl Parse for Function {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        parser.with_context("parsing function", |parser| {
            parser.keyword("int")?;
            let name = parser.parse()?;
            parser.expect_token(TokenKind::OpenParen)?;
            parser.accept_current();
            parser.expect_token(TokenKind::CloseParen)?;
            parser.accept_current();
            parser.expect_token(TokenKind::OpenBrace)?;
            parser.accept_current();
            parser.keyword("return")?;
            let return_expr = parser.parse()?;
            parser.expect_token(TokenKind::Semicolon)?;
            parser.accept_current();
            parser.expect_token(TokenKind::CloseBrace)?;
            parser.accept_current();
            Ok(Self { name, return_expr })
        })
    }
}
impl Parse for Identifier {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        parser.with_context("parsing identifier", |parser| {
            parser.expect_token(TokenKind::Identifier)?;
            let src = parser.current_token_source();
            parser.accept_current();
            Ok(Self(src.to_string()))
        })
    }
}
impl Parse for Expr {
    fn parse(parser: &mut Parser) -> ParseRes<Self> {
        parse_primary(parser)
            .and_then(|lhs| {
                parse_binary_expression(parser, lhs, 0)
                    .map_err(|e| e.add_context("parsing binary expression"))
            })
            .map_err(|x| x.add_context("parsing expression"))
    }
}

// parse prefix, simple or parenthesis
fn parse_primary(parser: &mut Parser) -> ParseRes<Expr> {
    parser.with_context("parsing primary expression", |parser| {
        // collect all unary operators
        let mut ops: Vec<_> = {
            let mut vec = Vec::new();
            while let Some(op) = parser
                .peek_token()?
                .and_then(TokenKind::as_operator)
                .and_then(UnaryOp::from_operator)
            {
                parser.accept_current();
                vec.push(op);
            }
            Ok(vec)
        }?;
        let mut expr = match parser.expect_a_token(Some(WantedSpec::Description("expression")))? {
            TokenKind::OpenParen => {
                parser.accept_current();
                let e = parser.parse()?;
                parser
                    .expect_token(TokenKind::CloseParen)
                    .map_err(|x| x.add_context("as the end of the expression"))?;
                parser.accept_current();
                Ok(e)
            }
            TokenKind::Number => {
                let num = parser.current_token_source().parse().unwrap();
                parser.accept_current();
                Ok(Expr::Constant(num))
            }
            tok => parser.reject_current_token(ParseErrorKind::Expected {
                found: tok,
                wanted: WantedSpec::Description("open paren or number"),
            }),
        }?;
        for operator in ops.drain(..).rev() {
            expr = Expr::Unary {
                operator,
                expr: Box::new(expr),
            }
        }
        Ok(expr)
    })
}

fn parse_binary_expression(
    parser: &mut Parser,
    mut lhs: Expr,
    min_precedence: u8,
) -> ParseRes<Expr> {
    while let Some(op) = parser
        .peek_token()?
        .and_then(TokenKind::as_operator)
        .and_then(BinaryOp::from_operator)
        .filter(|x| x.precedence() >= min_precedence)
    {
        parser.accept_current();
        let mut rhs = parse_primary(parser)?;
        while parser
            .peek_token()?
            .and_then(TokenKind::as_operator)
            .and_then(BinaryOp::from_operator)
            .filter(|op2| op2.precedence() > op.precedence())
            .is_some()
        {
            rhs = parse_binary_expression(parser, rhs, min_precedence + 1)?;
        }
        lhs = Expr::Binary {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };
    }
    Ok(lhs)
}
