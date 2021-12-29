use crate::error::{self, SourceMetadata, Span, WantedSpec};
use std::error::Error;
use std::fmt;

impl Error for LexErrorKind {}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::CloseBrace => write!(f, "closing brace '}}'"),
            Self::OpenBrace => write!(f, "opening brace '{{'"),
            Self::Identifier => write!(f, "identifier"),
            Self::Number => write!(f, "number"),
            Self::OpenParen => write!(f, "opening parentheses '('"),
            Self::CloseParen => write!(f, "closing parentheses ')'"),
            Self::Semicolon => write!(f, "semicolon ';'"),
            Self::Whitespace => write!(f, "whitespace"),
            Self::Operator { kind, has_equal } => write!(
                f,
                "operator `{}{}`",
                kind,
                if *has_equal { "=" } else { "" }
            ),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Operator::Plus => "+",
            Operator::Minus => "=",
            Operator::ExclamationMark => "!",
            Operator::Tilde => "~",
            Operator::Star => "*",
            Operator::Slash => "/",
            Operator::Percentage => "%",
            Operator::And => "&",
            Operator::Pipe => "|",
            Operator::Hat => "^",
            Operator::DoubleAnd => "&&",
            Operator::DoublePipe => "||",
            Operator::DoubleAngleRight => ">>",
            Operator::DoubleAngleLeft => "<<",
            Operator::AngleRight => ">",
            Operator::AngleLeft => "<",
            Operator::Equals => "=",
        })
    }
}

impl fmt::Display for LexErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedChar(ch) => write!(f, "unexpected {:?}", ch),
            Self::Expected { wanted, found } => {
                write!(f, "unexpected {:?}\nexpectetd {}", found, wanted)
            }
        }
    }
}
pub struct LexerIter<'a> {
    lexer: Lexer<'a>,
    eof: bool,
}

type LexError = error::Error<LexErrorKind>;

impl<'a> Iterator for LexerIter<'a> {
    type Item = Result<Token<'a>, LexError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.eof {
            None
        } else {
            let next = self.lexer.next_token();
            if matches!(next, Ok(None) | Err(_)) {
                self.eof = true;
            }
            next.transpose()
        }
    }
}

impl<'a> std::iter::FusedIterator for LexerIter<'a> {}

impl<'a> IntoIterator for Lexer<'a> {
    type IntoIter = LexerIter<'a>;
    type Item = <Self::IntoIter as Iterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        LexerIter {
            lexer: self,
            eof: false,
        }
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub source: Source<'a>,
}

impl<'a> Token<'a> {
    pub const fn new(kind: TokenKind, source: Source<'a>) -> Self {
        Self { kind, source }
    }
    pub const fn open_paren(source: Source<'a>) -> Self {
        Self::new(TokenKind::OpenParen, source)
    }
    pub const fn close_paren(source: Source<'a>) -> Self {
        Self::new(TokenKind::CloseParen, source)
    }
    pub const fn number(source: Source<'a>) -> Self {
        Self::new(TokenKind::Number, source)
    }
    pub const fn operator(kind: Operator, has_equal: bool, source: Source<'a>) -> Self {
        Self::new(TokenKind::Operator { kind, has_equal }, source)
    }
    pub const fn identifier(source: Source<'a>) -> Self {
        Self::new(TokenKind::Identifier, source)
    }
    pub const fn semi(source: Source<'a>) -> Self {
        Self::new(TokenKind::Semicolon, source)
    }
    pub const fn close_brace(source: Source<'a>) -> Self {
        Self::new(TokenKind::CloseBrace, source)
    }
    pub const fn open_brace(source: Source<'a>) -> Self {
        Self::new(TokenKind::OpenBrace, source)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Number,
    Identifier,
    Semicolon,
    Whitespace,
    Operator { kind: Operator, has_equal: bool },
}

impl TokenKind {
    pub const fn as_operator(self) -> Option<(Operator, bool)> {
        if let TokenKind::Operator { kind, has_equal } = self {
            Some((kind, has_equal))
        } else {
            None
        }
    }
}
// TODO: test that all operators are working from the lexer

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    ExclamationMark,
    Tilde,
    Star,
    Slash,
    Percentage,
    And,
    Pipe,
    Hat,
    DoubleAnd,
    DoublePipe,
    DoubleAngleRight,
    DoubleAngleLeft,
    AngleRight,
    AngleLeft,
    Equals,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Source<'source> {
    pub span: Span,
    pub source: &'source str,
}

impl const From<Source<'_>> for Span {
    fn from(s: Source<'_>) -> Self {
        s.span
    }
}

pub struct Lexer<'a> {
    input: std::iter::Peekable<std::str::CharIndices<'a>>,
    metadata: &'a SourceMetadata<'a>,
}

#[derive(Debug)]
pub enum LexErrorKind {
    Expected {
        wanted: error::WantedSpec<char>,
        found: char,
    },
    UnexpectedChar(char),
    // TODO
}

impl<'source> Lexer<'source> {
    pub fn new(input: &'source SourceMetadata<'source>) -> Self {
        Self {
            input: input.input().char_indices().peekable(),
            metadata: input,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token<'source>>, LexError> {
        self.skip_whitespace();
        if let Some(pos) = self.eat_char('(') {
            self.advance();
            return Ok(Some(Token::open_paren(self.source_from_len(pos, 1))));
        }
        if let Some(pos) = self.eat_char(')') {
            self.advance();
            return Ok(Some(Token::close_paren(self.source_from_len(pos, 1))));
        }
        if let Some(pos) = self.eat_char('{') {
            self.advance();
            return Ok(Some(Token::open_brace(self.source_from_len(pos, 1))));
        }
        if let Some(pos) = self.eat_char('}') {
            self.advance();
            return Ok(Some(Token::close_brace(self.source_from_len(pos, 1))));
        }
        if let Some(pos) = self.eat_char(';') {
            self.advance();
            return Ok(Some(Token::semi(self.source_from_len(pos, 1))));
        }
        if let Some(src) = self.identifier() {
            return Ok(Some(Token::identifier(src)));
        }
        if let Some((start, kind)) = self.operator() {
            let has_equal = self.skip_if(|x| x == '=').is_some();
            return Ok(Some(Token::operator(
                kind,
                has_equal,
                self.source_until_current(start),
            )));
        }
        if let Some(src) = self.number().map_err(|e| e.add_context("number"))? {
            return Ok(Some(Token::number(src)));
        }
        match self.input.peek().copied() {
            None => Ok(None),
            Some((pos, ch)) => Err(self.error(pos, LexErrorKind::UnexpectedChar(ch))),
        }
    }

    fn operator(&mut self) -> Option<(usize, Operator)> {
        let start = self.current_offset();
        let op = match self.input.peek()?.1 {
            '<' => {
                self.advance();
                if self.eat_char('<').is_some() {
                    self.advance();
                    Operator::DoubleAngleLeft
                } else {
                    Operator::AngleLeft
                }
            }
            '>' => {
                self.advance();
                if self.eat_char('>').is_some() {
                    self.advance();
                    Operator::DoubleAngleRight
                } else {
                    Operator::AngleRight
                }
            }
            '|' => {
                self.advance();
                if self.eat_char('|').is_some() {
                    self.advance();
                    Operator::DoublePipe
                } else {
                    Operator::Pipe
                }
            }
            '&' => {
                self.advance();
                if self.eat_char('&').is_some() {
                    self.advance();
                    Operator::DoubleAnd
                } else {
                    Operator::And
                }
            }
            '^' => {
                self.advance();
                Operator::Hat
            }
            '=' => {
                self.advance();
                Operator::Equals
            }
            '!' => {
                self.advance();
                Operator::ExclamationMark
            }
            '~' => {
                self.advance();
                Operator::Tilde
            }
            '+' => {
                self.advance();
                Operator::Plus
            }
            '-' => {
                self.advance();
                Operator::Minus
            }
            '/' => {
                self.advance();
                Operator::Slash
            }
            '*' => {
                self.advance();
                Operator::Star
            }
            '%' => {
                self.advance();
                Operator::Percentage
            }
            _ => return None,
        };
        Some((start, op))
    }

    fn identifier(&mut self) -> Option<Source<'source>> {
        let (start, _) = self.skip_if(|c| c.is_ascii_alphabetic() || c == '_')?;
        self.skip_while(|c| c.is_ascii_alphanumeric() || c == '_');
        Some(self.source_until_current(start))
    }

    fn number(&mut self) -> Result<Option<Source<'source>>, LexError> {
        let start = match self.skip_if(|c| c.is_ascii_digit()) {
            Some((pos, _)) => pos,
            None => return Ok(None),
        };
        self.skip_while(|c| c.is_ascii_digit());
        if let Some((pos, ch)) = self
            .input
            .peek()
            .filter(|(_, ch)| !is_delimeter(*ch))
            .copied()
        {
            return Err(self.error(
                pos,
                LexErrorKind::Expected {
                    wanted: WantedSpec::Description("delimeter or space after number"),
                    found: ch,
                },
            ));
        }
        Ok(Some(self.source_until_current(start)))
    }

    fn skip_while<F>(&mut self, filter: F) -> Source<'source>
    where
        F: Fn(char) -> bool,
    {
        let current = self.current_offset();
        while self.input.peek().filter(|(_, ch)| filter(*ch)).is_some() {
            self.input.next();
        }
        self.source_until_current(current)
    }

    fn skip_if<F>(&mut self, filter: F) -> Option<(usize, char)>
    where
        F: Fn(char) -> bool,
    {
        let (pos, ch) = *self.input.peek()?;
        if filter(ch) {
            self.advance();
            Some((pos, ch))
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) -> Option<Source<'source>> {
        let current = self.current_offset();
        let mut last_pos = current;
        while let Some((pos, _)) = self
            .input
            .peek()
            .filter(|(_, ch)| ch.is_whitespace())
            .copied()
        {
            self.advance();
            last_pos = pos;
        }
        if last_pos == current {
            None
        } else {
            Some(self.source_from(current, last_pos))
        }
    }

    fn advance(&mut self) {
        self.input.next();
    }

    fn source_until_current(&mut self, start: usize) -> Source<'source> {
        let current = self.current_offset();
        self.source_from(start, current)
    }

    fn source_from(&self, start: usize, end: usize) -> Source<'source> {
        Source {
            span: Span {
                offset: start,
                len: end - start,
            },
            source: &self.metadata.input()[start..end],
        }
    }

    fn source_from_len(&self, start: usize, len: usize) -> Source<'source> {
        self.source_from(start, start + len)
    }

    fn eat_char(&mut self, ch: char) -> Option<usize> {
        self.input
            .peek()
            .filter(|(_, x)| *x == ch)
            .map(|(pos, _)| *pos)
    }

    fn error(&self, position: usize, kind: LexErrorKind) -> LexError {
        LexError::new(kind).with_source(Span::new(position), self.metadata)
    }

    pub fn current_span(&mut self) -> Span {
        Span::new(self.current_offset())
    }

    pub const fn get_metadata(&self) -> &SourceMetadata {
        self.metadata
    }

    fn current_offset(&mut self) -> usize {
        self.input
            .peek()
            .map(|(x, _)| *x)
            .unwrap_or_else(|| self.metadata.input().len())
    }
}
#[inline]
fn is_delimeter(ch: char) -> bool {
    ch.is_whitespace() || ch.is_ascii_punctuation()
}
