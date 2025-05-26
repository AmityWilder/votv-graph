use std::{range::Range, str::pattern::Pattern};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType<'a> {
    Unknown(&'a str),
    Ident(&'a str),
    Bool(bool),
    Int(isize),
    Count(usize),
    Float(f32),
    Dot,
    Colon,
    Slash,
    Pipe,
    Comma,
    ParenOpen,
    ParenClose,
}

impl TokenType<'_> {
    pub const PUNC: [&'static str; 7] = [".", ":", "/", "|", ",", "(", ")"];
}

impl std::fmt::Display for TokenType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown(s) => s.fmt(f),
            Self::Ident  (s) => s.fmt(f),
            Self::Bool   (b) => b.fmt(f),
            Self::Int    (i) => i.fmt(f),
            Self::Count  (n) => n.fmt(f),
            Self::Float  (x) => x.fmt(f),
            Self::Dot        => '.'.fmt(f),
            Self::Colon      => ':'.fmt(f),
            Self::Slash      => '/'.fmt(f),
            Self::Pipe       => '|'.fmt(f),
            Self::Comma      => ','.fmt(f),
            Self::ParenOpen  => '('.fmt(f),
            Self::ParenClose => ')'.fmt(f),
        }
    }
}

impl<'a> TokenType<'a> {
    pub fn new(s: &'a str) -> Option<Self> {
        if s.starts_with(|c: char| c.is_alphabetic()) {
            if let Ok(b) = s.parse() {
                return Some(Self::Bool(b));
            } else if s.chars().all(|ch: char| ch.is_alphanumeric() || ch == '_') {
                return Some(Self::Ident(s));
            }
        } else if s.strip_prefix('-').unwrap_or(s).starts_with(|c: char| c.is_ascii_digit()) {
            if let Ok(n) = s.parse() {
                return Some(Self::Count(n));
            } else if let Ok(i) = s.parse() {
                return Some(Self::Int(i));
            } else if let Ok(f) = s.parse() {
                return Some(Self::Float(f));
            }
        } else {
            match s {
                "." => return Some(Self::Dot),
                ":" => return Some(Self::Colon),
                "/" => return Some(Self::Slash),
                "|" => return Some(Self::Pipe),
                "," => return Some(Self::Comma),
                "(" => return Some(Self::ParenOpen),
                ")" => return Some(Self::ParenClose),
                _ => {}
            }
        }
        None
    }
}

#[derive(Debug)]
pub enum LexerError<'a> {
    UnknownToken(&'a str),
}
impl<'a> std::fmt::Display for LexerError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnknownToken(token) => write!(f, "unexpected token in input: {token}"),
        }
    }
}
impl<'a> std::error::Error for LexerError<'a> {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub value: TokenType<'a>,
    pub range: Range<usize>,
}

impl<'a> Token<'a> {
    #[inline]
    fn unknown(src: &'a str, token: &'a str) -> Self {
        Self {
            value: TokenType::Unknown(token),
            range: Range::from(src.substr_range(token).unwrap()),
        }
    }

    #[inline]
    fn ident(src: &'a str, token: &'a str) -> Self {
        Self {
            value: TokenType::Ident(token),
            range: Range::from(src.substr_range(token).unwrap()),
        }
    }

    #[inline]
    fn bool(src: &'a str, token: &'a str, value: bool) -> Self {
        Self {
            value: TokenType::Bool(value),
            range: Range::from(src.substr_range(token).unwrap()),
        }
    }

    #[inline]
    fn int(src: &'a str, token: &'a str, value: isize) -> Self {
        Self {
            value: TokenType::Int(value),
            range: Range::from(src.substr_range(token).unwrap()),
        }
    }

    #[inline]
    fn count(src: &'a str, token: &'a str, value: usize) -> Self {
        Self {
            value: TokenType::Count(value),
            range: Range::from(src.substr_range(token).unwrap()),
        }
    }

    #[inline]
    fn float(src: &'a str, token: &'a str, value: f32) -> Self {
        Self {
            value: TokenType::Float(value),
            range: Range::from(src.substr_range(token).unwrap()),
        }
    }

    #[inline]
    fn punc(src: &'a str, token: &'a str, value: TokenType<'a>) -> Self {
        Self {
            value,
            range: Range::from(src.substr_range(token).unwrap()),
        }
    }
}

pub struct SplitAround<'a, P: Pattern> {
    iter: std::str::SplitInclusive<'a, P>,
    pat: P,
    delim: Option<&'a str>,
}

impl<'a, P: Pattern + Copy> SplitAround<'a, P> {
    fn new(iter: &'a str, pat: P) -> Self {
        Self {
            iter: iter.split_inclusive(pat),
            pat,
            delim: None,
        }
    }
}

pub trait SplitAroundExt {
    fn split_around<P: Pattern + Copy>(&self, pat: P) -> SplitAround<'_, P>;
}

impl SplitAroundExt for str {
    #[inline]
    fn split_around<P: Pattern + Copy>(&self, pat: P) -> SplitAround<'_, P> {
        SplitAround::new(self, pat)
    }
}

impl<'a, P: Pattern + Copy> Iterator for SplitAround<'a, P> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let elt @ Some(_) = self.delim.take() {
            return elt;
        }
        self.iter.next()
            .map(|mut s| {
                if let Some(mid) = s.find(self.pat) {
                    let delim;
                    (s, delim) = s.split_at(mid);
                    self.delim = Some(delim)
                }
                s
            })
    }
}

pub fn lex(src: &str) -> Vec<Token<'_>> {
    let mut result = Vec::new();
    src.split
}

#[cfg(test)]
mod lexer_tests {
    use std::assert_matches::assert_matches;
    use super::*;

    #[test]
    fn test0() {
        let tokens = lex("sv.add squeaksqueak \"squeak squeak squeak\" x:63.7/y:97.2").expect("parse should succeed");
        assert_matches!(&tokens[..], [
            Token { value: TokenType::Ident("sv"), .. },
            Token { value: TokenType::Ident("add"), .. },
            Token { value: TokenType::Ident("x"), .. },
            Token { value: TokenType::Colon, .. },
            Token { value: TokenType::Float(63.7), .. },
            Token { value: TokenType::Slash, .. },
            Token { value: TokenType::Ident("y"), .. },
            Token { value: TokenType::Colon, .. },
            Token { value: TokenType::Float(97.2), .. },
        ]);
    }
}
