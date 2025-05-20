use std::ops::{Deref, DerefMut, Range};

#[derive(Debug)]
pub enum Punctuation {
    /// `:`
    Colon,
    /// `/`
    Slash,
    /// `|`
    Pipe,
    /// `,`
    Comma,
    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,
}

impl std::fmt::Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Colon      => ":",
            Self::Slash      => "/",
            Self::Pipe       => "|",
            Self::Comma      => ",",
            Self::ParenOpen  => "(",
            Self::ParenClose => ")",
        }.fmt(f)
    }
}

impl Punctuation {
    pub fn new(s: &str) -> Option<Self> {
        match s {
            ":" => Some(Self::Colon),
            "/" => Some(Self::Slash),
            "|" => Some(Self::Pipe),
            "," => Some(Self::Comma),
            "(" => Some(Self::ParenOpen),
            ")" => Some(Self::ParenClose),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum TokenType<'a> {
    Text(&'a str),
    Punc(Punctuation),
    Bool(bool),
    Int(isize),
    Count(usize),
    Float(f32),
}

impl std::fmt::Display for TokenType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Text (t) => t.fmt(f),
            TokenType::Punc (p) => p.fmt(f),
            TokenType::Bool (b) => b.fmt(f),
            TokenType::Int  (i) => i.fmt(f),
            TokenType::Count(n) => n.fmt(f),
            TokenType::Float(x) => x.fmt(f),
        }
    }
}

impl<'a> TokenType<'a> {
    pub fn new(s: &'a str) -> Self {
        if s.starts_with(|c: char| c.is_ascii_alphabetic()) {
            if let Ok(b) = s.parse() {
                return Self::Bool(b);
            }
        } else if s.strip_prefix('-').unwrap_or(s).starts_with(|c: char| c.is_ascii_digit()) {
            if let Ok(n) = s.parse() {
                return Self::Count(n);
            } else if let Ok(i) = s.parse() {
                return Self::Int(i);
            } else if let Ok(f) = s.parse() {
                return Self::Float(f);
            }
        } else if let Some(p) = Punctuation::new(s) {
            return Self::Punc(p);
        }
        return Self::Text(s);
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    pub value: TokenType<'a>,
    pub range: Range<usize>,
}

pub fn tokenize(src: &str) -> Vec<Token<'_>> {
    src .split_whitespace()
        .flat_map(|s| s
            .split_inclusive(':')
            .flat_map(|s| {
                let (a, b) = s.split_at(s.rfind(':').expect("msg"));
                [a, b].into_iter()
            })
        )
        .map(|value| Token {
            range: src.substr_range(value).expect("split_whitespace should provide substrings of its argument"),
            value: TokenType::new(value),
        })
        .collect()
}

pub struct Sequence<T, S> {
    items: Vec<T>,
    separators: Vec<S>,
}

impl<T: std::fmt::Debug> std::fmt::Debug for Sequence<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("Sequence");
        for pair in self.0.chunks(2) {
            f.field("item", &pair[0]);
            if let Some(sep) = pair.get(1) {
                f.field("separator", sep);
            }
        }
        f.finish()
    }
}

impl<T> Sequence<T> {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn items(&self) -> &[T] {
        &self.items
    }

    pub fn separators(&self) -> &[T] {
        &self.items
    }
}

#[derive(Debug)]
pub struct Field<'a> {
    field: Token<'a>,
    colon: Token<'a>,
    value: SyntaxNode<'a>,
}

#[derive(Debug)]
pub enum SyntaxNode<'a> {
    Token(Token<'a>),
    Group {
        open: Token<'a>,
        items: Vec<SyntaxNode<'a>>,
        close: Token<'a>,
    },
    Sequence(Sequence<SyntaxNode<'a>>),
    Struct(Vec<Field<'a>>),
}

#[derive(Debug)]
pub enum ParseErrorType {
    UnclosedGroup(&'static str),
}

#[derive(Debug)]
pub struct ParseError<'a, 'b> {
    pub src: &'a [Token<'b>],
    pub kind: ParseErrorType,
}

impl std::fmt::Display for ParseError<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tokens = self.src.iter()
            .map(|t| t.value.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        match self.kind {
            ParseErrorType::UnclosedGroup(expected) => write!(f, "unclosed group: expected `{tokens}` to end with `{expected}`"),
        }
    }
}

impl std::error::Error for ParseError<'_, '_> {}

pub fn parse<'a, 'b>(tokens: &'a [Token<'b>]) -> Result<Vec<SyntaxNode<'b>>, ParseError<'a, 'b>> {
    let mut stack = vec![Vec::new()];
    for token in tokens {
        if matches!(&token.value, TokenType::Punc(Punctuation::ParenOpen)) {
            stack.push(Vec::new());
        } else if matches!(&token.value, TokenType::Punc(Punctuation::ParenClose)) {
            let top = stack.pop().unwrap();

            continue;
        }
        stack.last_mut().unwrap().push(token);
    }
    let result = stack.pop().unwrap();
    assert!(stack.is_empty());
    Ok(result)
}
