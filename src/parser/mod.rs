use crate::error::*;
use crate::lexer::*;
mod expr;
mod function;
mod identifier;
mod program;

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

    /// Iterates the same parser until a failure happens. The [`Err`] variant
    /// is used only for lexing errors, the rest will only trigger a [`None`]
    pub fn iterate<T: Parse>(&'a mut self) -> impl Iterator<Item = ParseRes<T>> + 'a {
        let mut had_err = false;
        std::iter::from_fn(move || {
            if had_err {
                None
            } else {
                match T::parse(self) {
                    Err(p) if p.kind.is_critical() => {
                        had_err = true;
                        Some(Err(p))
                    }
                    Err(_) => None,
                    ok => Some(ok),
                }
            }
        })
        .fuse()
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

impl ParseErrorKind {
    pub const fn is_critical(&self) -> bool {
        matches!(self, Self::LexError(_))
    }
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
