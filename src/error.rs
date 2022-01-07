use std::error;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Error<T> {
    pub kind: T,
    file: Option<std::path::PathBuf>,
    snippet: Option<Snippet>,
    contexts: Vec<&'static str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub offset: usize,
    pub len: usize,
}

impl Span {
    pub const fn new(offset: usize) -> Self {
        Self { offset, len: 1 }
    }
    pub const fn as_range(&self) -> std::ops::Range<usize> {
        self.offset..self.offset + self.len
    }
    pub fn snippet_from_source(&self, source: &SourceMetadata) -> Option<Snippet> {
        let mut offset = 0;
        for (i, line) in source.input().split_terminator('\n').enumerate() {
            let next_offset = offset + line.len() + 1;
            if next_offset >= self.offset {
                // offset is somewhere in the current line
                return Some(Snippet {
                    position: Position {
                        line: i,
                        col: self.offset - offset,
                    },
                    line: line.to_string(),
                });
            }
            offset = next_offset;
        }
        None
    }
}

#[derive(Debug)]
pub struct SourceMetadata<'a> {
    file: Option<std::path::PathBuf>,
    source: &'a str,
}

impl<'a> SourceMetadata<'a> {
    pub const fn input(&self) -> &'a str {
        self.source
    }
    pub const fn new(source: &'a str) -> Self {
        Self { file: None, source }
    }
    #[must_use]
    pub fn with_file(mut self, file: std::path::PathBuf) -> Self {
        self.file = Some(file);
        self
    }
}

impl<T> Error<T> {
    pub const fn new(kind: T) -> Self {
        Self {
            kind,
            snippet: None,
            file: None,
            contexts: Vec::new(),
        }
    }
    pub fn map_kind<F, U>(self, mapper: F) -> Error<U>
    where
        F: Fn(T) -> U,
    {
        Error {
            kind: mapper(self.kind),
            snippet: self.snippet,
            file: self.file,
            contexts: self.contexts,
        }
    }
    /// The source given is only applied if there was no additional source
    #[must_use]
    pub fn with_backup_source(self, span: Span, source: &SourceMetadata) -> Self {
        if self.file.is_some() {
            self
        } else {
            self.with_source(span, source)
        }
    }
    #[must_use]
    pub fn with_source(mut self, span: Span, source: &SourceMetadata) -> Self {
        self.file = source.file.clone();
        self.snippet = span.snippet_from_source(source);
        self
    }
    #[must_use]
    pub fn add_context(mut self, ctx: &'static str) -> Self {
        self.contexts.push(ctx);
        self
    }
}

#[derive(Debug, Clone)]
pub struct Snippet {
    position: Position,
    line: String,
}

#[derive(Debug)]
pub enum WantedSpec<T> {
    Specific(T),
    Description(&'static str),
}

impl<T: fmt::Display> fmt::Display for WantedSpec<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Description(desc) => f.write_str(desc),
            Self::Specific(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub col: usize,
    pub line: usize,
}

impl<T: error::Error + 'static> error::Error for Error<T> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&self.kind)
    }
}

impl<T: fmt::Display> fmt::Display for Error<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let whiles = self
            .contexts
            .iter()
            .copied()
            .fold(String::new(), |acc, next| acc + "\nwhile " + next);
        let snippet = if let Some(snip) = &self.snippet {
            snip
        } else {
            return write!(f, "{}(no location info){}", self.kind, whiles);
        };
        let file = self
            .file
            .as_ref()
            .and_then(|x| x.to_str())
            .unwrap_or("<unknown source>");

        write!(
            f,
            "\
{kind}
   --> {file}:{line}:{col}
    |
{line:3} | {snippet}
    | {marker:>0$}{whiles}",
            snippet.position.col + 1,
            marker = '^',
            line = snippet.position.line + 1,
            col = snippet.position.col + 1,
            file = file,
            kind = self.kind,
            snippet = snippet.line,
            whiles = whiles,
        )
    }
}
