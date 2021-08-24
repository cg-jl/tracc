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
            Self::Operator { kind } => write!(f, "operator `{}`", kind),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ExclamationMark => write!(f, "!"),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Tilde => write!(f, "~"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::DoubleAnd => write!(f, "&&"),
            Self::DoublePipe => write!(f, "||"),
        }
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
    pub const fn operator(kind: Operator, source: Source<'a>) -> Self {
        Self::new(TokenKind::Operator { kind }, source)
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
    Operator { kind: Operator },
}

impl TokenKind {
    pub const fn as_operator(self) -> Option<Operator> {
        if let TokenKind::Operator { kind } = self {
            Some(kind)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    ExclamationMark,
    Tilde,
    Star,
    Slash,
    DoubleAnd,
    DoublePipe,
}

#[derive(Debug)]
pub struct Source<'a> {
    pub span: Span,
    pub source: &'a str,
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

impl<'a> Lexer<'a> {
    pub fn new(input: &'a SourceMetadata<'a>) -> Self {
        Self {
            input: input.input().char_indices().peekable(),
            metadata: input,
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token<'a>>, LexError> {
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
            return Ok(Some(Token::operator(
                kind,
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

    fn choice<T>(&mut self, choices: &[&dyn Fn(&mut Self) -> Option<T>]) -> Option<T> {
        for x in choices {
            if let Some(t) = x(self) {
                return Some(t);
            }
        }
        None
    }

    fn operator(&mut self) -> Option<(usize, Operator)> {
        let start = self.current_offset();
        let op: Operator = self.choice::<Operator>(&[
            &|lexer: &mut Self| lexer.eat_char('+').map(|_| Operator::Plus),
            &|lexer: &mut Self| lexer.eat_char('-').map(|_| Operator::Minus),
            &|lexer: &mut Self| lexer.eat_char('!').map(|_| Operator::ExclamationMark),
            &|lexer: &mut Self| lexer.eat_char('~').map(|_| Operator::Tilde),
            &|lexer: &mut Self| lexer.eat_char('*').map(|_| Operator::Star),
            &|lexer: &mut Self| lexer.eat_char('/').map(|_| Operator::Slash),
            &|lexer: &mut Self| lexer.eat_str("&&").map(|_| Operator::DoubleAnd),
            &|lexer: &mut Self| lexer.eat_str("||").map(|_| Operator::DoublePipe),
        ])?;
        self.advance();
        Some((start, op))
    }

    fn identifier(&mut self) -> Option<Source<'a>> {
        let (start, _) = self.skip_if(|c| c.is_ascii_alphabetic() || c == '_')?;
        self.skip_while(|c| c.is_ascii_alphanumeric() || c == '_');
        Some(self.source_until_current(start))
    }

    fn number(&mut self) -> Result<Option<Source<'a>>, LexError> {
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

    fn eat_str(&mut self, str: &str) -> Option<usize> {
        let current_offset = self.current_offset();
        if self.metadata.input()[current_offset..].starts_with(str) {
            for _ in str.chars() {
                self.advance();
            }
            Some(current_offset)
        } else {
            None
        }
    }

    fn skip_while<F>(&mut self, filter: F) -> Source<'a>
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

    fn skip_whitespace(&mut self) -> Option<Source<'a>> {
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

    fn source_until_current(&mut self, start: usize) -> Source<'a> {
        let current = self.current_offset();
        self.source_from(start, current)
    }

    fn source_from(&self, start: usize, end: usize) -> Source<'a> {
        Source {
            span: Span::new(start),
            source: &self.metadata.input()[start..end],
        }
    }

    fn source_from_len(&self, start: usize, len: usize) -> Source<'a> {
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
