use std::str::pattern::{Pattern, Searcher};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Ident,
    Text,
    Number,
    Punc(char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub src: &'a str,
}

impl PartialEq<str> for Token<'_> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.src == other
    }
}

impl PartialEq<char> for Token<'_> {
    #[inline]
    fn eq(&self, other: &char) -> bool {
        self.eq_punc(*other)
    }
}

impl<'a> Token<'a> {
    pub const PUNCTUATION: [char; 8] = ['.', ',', '|', ':', '/', '#', '(', ')'];

    pub const fn ident(src: &'a str) -> Self {
        Self { kind: TokenType::Ident, src }
    }
    pub const fn text(src: &'a str) -> Self {
        Self { kind: TokenType::Text, src }
    }
    pub const fn number(src: &'a str) -> Self {
        Self { kind: TokenType::Number, src }
    }
    pub const fn punc(src: &'a str, ch: char) -> Self {
        Self { kind: TokenType::Punc(ch), src }
    }

    #[inline]
    pub const fn is_ident(&self) -> bool {
        matches!(self.kind, TokenType::Ident)
    }

    #[inline]
    pub const fn is_text(&self) -> bool {
        matches!(self.kind, TokenType::Text)
    }

    #[inline]
    pub const fn is_str(&self) -> bool {
        matches!(self.kind, TokenType::Ident | TokenType::Text)
    }

    #[inline]
    pub const fn is_number(&self) -> bool {
        matches!(self.kind, TokenType::Number)
    }

    #[inline]
    pub const fn is_punc(&self) -> bool {
        matches!(self.kind, TokenType::Punc(_))
    }

    #[inline]
    pub const fn eq_punc(&self, value: char) -> bool {
        matches!(self.kind, TokenType::Punc(ch) if ch == value)
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    src: &'a str,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.src = self.src.trim_start();
        if self.src.is_empty() { return None; }

        let len = self.src.len();
        let (data, mid) =
            if self.src.starts_with(|ch: char| ch.is_alphabetic() || ch == '_') {
                (TokenType::Ident, (|ch: char| ch.is_alphanumeric() || ch == '_')
                    .into_searcher(self.src)
                    .next_reject()
                    .map_or(len, |(i, _)| i))
            } else if self.src.strip_prefix('-').unwrap_or(self.src).starts_with(char::is_numeric) {
                let mut is_start = true;
                let mut has_dot = false;
                (TokenType::Number, (|ch: char| {
                    if is_start {
                        is_start = false;
                        if ch == '-' { return true; }
                    }
                    if ch == '.' {
                        !std::mem::replace(&mut has_dot, true)
                    } else {
                        ch.is_alphanumeric() || ch == '_'
                    }
                }).into_searcher(self.src)
                    .next_reject()
                    .map_or(len, |(i, _)| i))
            } else if let Some(ch) = Token::PUNCTUATION.into_iter().find(|&ch| self.src.starts_with(ch)) {
                (TokenType::Punc(ch), ch.len_utf8())
            } else {
                if let Some(delim) = ['\'', '"'].into_iter().find(|&delim| self.src.starts_with(delim)) {
                    let delim_len = delim.len_utf8();
                    let src = &self.src[delim_len..];
                    let mid = src.find(delim).unwrap_or(src.len());
                    let (front, back) = (&src[..mid], &src[mid + delim_len..]);
                    self.src = back;
                    return Some(Token { kind: TokenType::Text, src: front });
                }
                (TokenType::Text, self.src.find(|ch: char| ch.is_whitespace()).unwrap_or(len))
            };

        let (front, back) = self.src.split_at(mid);
        self.src = back;
        Some(Token { kind: data, src: front })
    }
}

impl std::iter::FusedIterator for Lexer<'_> {}

pub fn lex(src: &str) -> Lexer<'_> {
    Lexer { src }
}

#[derive(Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct AdjTokens<'a>([Token<'a>]);

impl<'a> ToOwned for AdjTokens<'a> {
    type Owned = Box<Self>;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        let inner = Box::into_raw(self.0.to_owned().into_boxed_slice());
        // SAFETY: AdjTokens is just a wrapper of [Token<'a>],
        // therefore converting Box<[Token<'a>]> to Box<AdjTokens> is safe.
        unsafe { Box::from_raw(inner as *mut [Token<'a>] as *mut Self) }
    }
}

impl<'a> std::ops::Deref for AdjTokens<'a> {
    type Target = [Token<'a>];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> AdjTokens<'a> {
    /// # Safety
    ///
    /// - The `src` field of every token must immediately follow that of the previous element.
    /// - Elements cannot be modified for the lifetime of of this object.
    /// - Every element must be valid.
    #[inline]
    const unsafe fn new_unchecked<'b>(tokens: &'b [Token<'a>]) -> &'b Self {
        // SAFETY: AdjTokens is just a wrapper of [Token<'a>],
        // therefore converting &[Token<'a>] to &AdjTokens is safe.
        unsafe { &*(tokens as *const [Token<'a>] as *const Self) }
    }

    /// Return the concatenation of every token's `src` in the object.
    /// Returns [`None`] if the slice is empty.
    #[inline]
    pub const fn to_str(&self) -> Option<&'a str> {
        if let Some(first) = self.0.first() {
            unsafe {
                // the existence of a first element guarantees the existence of a last element (they would be the same element).
                let last = self.0.last().unwrap_unchecked();
                let start = first.src.as_ptr();
                // "Allocated objects can never be larger than `isize::MAX` bytes, so if the computed offset
                // stays in bounds of the allocated object, it is guaranteed to satisfy the first requirement.
                // This implies, for instance, that `vec.as_ptr().add(vec.len())` (for `vec: Vec<T>`) is always
                // safe." --std::ptr::add documentation
                let end = last.src.as_ptr().add(std::mem::size_of_val(last.src));
                // if the safety of the constructor is upheld, `end` is guaranteed to come after `start`.
                let len = end.offset_from_unsigned(start);
                // if the safety of the constructor is upheld, every src is guaranteed to be valid and consecutive in memory.
                let bytes = std::slice::from_raw_parts(start, len);
                // because the source is a str, its elements are guaranteed to be valid utf8.
                let s = str::from_utf8_unchecked(bytes);
                Some(s)
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AdjChunks<'a, 'b> {
    tokens: &'b [Token<'a>],
}

impl<'a, 'b> AdjChunks<'a, 'b> {
    fn new(tokens: &'b [Token<'a>]) -> Self {
        Self { tokens }
    }
}

impl<'a, 'b> Iterator for AdjChunks<'a, 'b> {
    type Item = &'b AdjTokens<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let mut it = self.tokens
            .iter()
            .map(|token| token.src.as_bytes().as_ptr_range());

        if let Some(mut prev) = it.next() {
            let mut mid = 1;
            for curr in it {
                assert!(prev.end <= curr.start);
                if prev.end == curr.start {
                    mid += 1;
                    prev = curr;
                } else {
                    break;
                }
            }

            let (front, back) = self.tokens.split_at(mid);
            self.tokens = back;
            // Safety: Just confirmed the tokens are consecutive
            let chunk = unsafe { AdjTokens::new_unchecked(front) };
            Some(chunk)
        } else {
            None
        }
    }
}

impl std::iter::FusedIterator for AdjChunks<'_, '_> {}

#[derive(Debug, Clone, PartialEq)]
pub struct SplitArgs<'a, 'b> {
    tokens: &'b [Token<'a>],
}

impl<'a, 'b> SplitArgs<'a, 'b> {
    fn new(tokens: &'b [Token<'a>]) -> Self {
        Self { tokens }
    }
}

impl<'a, 'b> Iterator for SplitArgs<'a, 'b> {
    type Item = &'b [Token<'a>];

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(a) = self.tokens.get(0) {
            let mut len = 1;
            if let Some(b) = self.tokens.get(1) {
                if a.eq_punc('#') {
                    // hex code
                    len = 2;
                } else if a.is_ident() && b.eq_punc('.') {
                    len = 1 + self.tokens[1..]
                        .chunks_exact(2)
                        .map(|x| [x[0], x[1]])
                        .take_while(|[a, b]| a.eq_punc('.') && b.is_ident())
                        .count()*2;
                } else if a.is_ident() && b.eq_punc(':') {
                    // struct
                    len = (0..4)
                        .cycle()
                        .zip(self.tokens)
                        .take_while(|&(state, tkn)| match state {
                            0 => tkn.is_ident(),
                            1 => tkn.eq_punc(':'),
                            2 => !tkn.is_punc(),
                            3 => tkn.eq_punc('/'),
                            _ => unreachable!(),
                        })
                        .count();
                } else if a.is_ident() && b.eq_punc('(') {
                    // group
                    len = self.tokens.iter()
                        .position(|tkn| tkn.eq_punc(')'))
                        .map_or(self.tokens.len(), |n| n + 1);
                }
            }

            let (front, back) = self.tokens.split_at(len);
            self.tokens = back;
            Some(front)
        } else {
            None
        }
    }
}

impl std::iter::FusedIterator for SplitArgs<'_, '_> {}

pub trait TokenSliceExt<'a> {
    fn adj_chunks<'b>(&'b self) -> AdjChunks<'a, 'b>;
    fn split_args<'b>(&'b self) -> SplitArgs<'a, 'b>;
}

impl<'a> TokenSliceExt<'a> for [Token<'a>] {
    #[inline]
    fn adj_chunks<'b>(&'b self) -> AdjChunks<'a, 'b> {
        AdjChunks::new(self)
    }

    #[inline]
    fn split_args<'b>(&'b self) -> SplitArgs<'a, 'b> {
        SplitArgs::new(self)
    }
}

#[derive(Debug, Clone)]
pub struct Syntax<'a, 'b> {
    tokens: &'b [Token<'a>],
    sep: Option<&'b [Token<'a>]>,
}

impl<'a, 'b> Syntax<'a, 'b> {
    fn new(tokens: &'b [Token<'a>]) -> Self {
        Self { tokens, sep: None }
    }
}

impl<'a, 'b> Iterator for Syntax<'a, 'b> {
    type Item = &'b [Token<'a>];

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(sep) = self.sep.take() {
            return Some(sep);
        } else if self.tokens.is_empty() {
            return None;
        }

        let front = if let Some(mid) = self.tokens.iter().position(|tkn| tkn.eq_punc('|')) {
            let front = &self.tokens[..mid];
            self.sep = Some(&self.tokens[mid..mid + 1]);
            self.tokens = &self.tokens[mid + 1..];
            front
        } else {
            let (front, back) = self.tokens.split_at(self.tokens.len());
            self.tokens = back;
            front
        };

        Some(front)
    }
}

impl std::iter::FusedIterator for Syntax<'_, '_> {}

pub fn syntax<'a, 'b>(tokens: &'b [Token<'a>]) -> Syntax<'a, 'b> {
    Syntax::new(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod lex {
        use super::*;

        #[test]
        fn test_identify_ident() {
            let mut it = lex("squeak");
            assert_eq!(it.next(), Some(Token::ident("squeak")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_split_spaces() {
            let mut it = lex("apple orange");
            assert_eq!(it.next(), Some(Token::ident("apple")));
            assert_eq!(it.next(), Some(Token::ident("orange")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_messy_spaces() {
            let mut it = lex("    apple   orange      ");
            assert_eq!(it.next(), Some(Token::ident("apple")));
            assert_eq!(it.next(), Some(Token::ident("orange")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_ident_with_underscores() {
            let mut it = lex("gfs_fhgdi__gfsd_");
            assert_eq!(it.next(), Some(Token::ident("gfs_fhgdi__gfsd_")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_ident_with_numbers() {
            let mut it = lex("gdfjh64573");
            assert_eq!(it.next(), Some(Token::ident("gdfjh64573")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_num_with_letters() {
            let mut it = lex("5gdfjh64573");
            assert_eq!(it.next(), Some(Token::number("5gdfjh64573")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_num() {
            let mut it = lex("65436");
            assert_eq!(it.next(), Some(Token::number("65436")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_num_with_decimal() {
            let mut it = lex("76.37");
            assert_eq!(it.next(), Some(Token::number("76.37")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_num_with_neg() {
            let mut it = lex("-563");
            assert_eq!(it.next(), Some(Token::number("-563")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_num_with_neg_and_decimal() {
            let mut it = lex("-768.67");
            assert_eq!(it.next(), Some(Token::number("-768.67")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_split_around_punc() {
            let mut it = lex("apple|orange.banana/pineapple(x:a,y)");
            assert_eq!(it.next(), Some(Token::ident("apple")));
            assert_eq!(it.next(), Some(Token::punc("|", '|')));
            assert_eq!(it.next(), Some(Token::ident("orange")));
            assert_eq!(it.next(), Some(Token::punc(".", '.')));
            assert_eq!(it.next(), Some(Token::ident("banana")));
            assert_eq!(it.next(), Some(Token::punc("/", '/')));
            assert_eq!(it.next(), Some(Token::ident("pineapple")));
            assert_eq!(it.next(), Some(Token::punc("(", '(')));
            assert_eq!(it.next(), Some(Token::ident("x")));
            assert_eq!(it.next(), Some(Token::punc(":", ':')));
            assert_eq!(it.next(), Some(Token::ident("a")));
            assert_eq!(it.next(), Some(Token::punc(",", ',')));
            assert_eq!(it.next(), Some(Token::ident("y")));
            assert_eq!(it.next(), Some(Token::punc(")", ')')));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_text() {
            let mut it = lex("apple -fgd @hgf^7$");
            assert_eq!(it.next(), Some(Token::ident("apple")));
            assert_eq!(it.next(), Some(Token::text("-fgd")));
            assert_eq!(it.next(), Some(Token::text("@hgf^7$")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_text_with_double_quotes() {
            let mut it = lex("apple \"-fgd @hgf^7$\" x");
            assert_eq!(it.next(), Some(Token::ident("apple")));
            assert_eq!(it.next(), Some(Token::text("-fgd @hgf^7$")));
            assert_eq!(it.next(), Some(Token::ident("x")));
            assert_eq!(it.next(), None);
        }

        #[test]
        fn test_identify_text_with_single_quotes() {
            let mut it = lex("apple '-fgd @hgf^7$' x");
            assert_eq!(it.next(), Some(Token::ident("apple")));
            assert_eq!(it.next(), Some(Token::text("-fgd @hgf^7$")));
            assert_eq!(it.next(), Some(Token::ident("x")));
            assert_eq!(it.next(), None);
        }
    }

    mod syntax {
        use super::*;

        macro_rules! assert_syntax {
            ($value:expr, $outer:pat, $inner:block) => {
                match $value {
                    $outer => $inner
                    x => panic!("assertion failed\n  left: {x:?}\n right: {}", stringify!($outer))
                }
            };
        }

        #[test]
        fn test_lone_ident_is_cmd() {
            let tokens = lex("apple").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[Token::ident("apple")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_pipeline() {
            let tokens = lex("apple | orange").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[Token::ident("apple")][..]));
                assert_eq!(args.next(), None);
            });
            assert_eq!(it.next(), Some(&[Token::punc("|", '|')][..]));
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[Token::ident("orange")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_command_with_dots() {
            let tokens = lex("apple.orange.banana").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[
                    Token::ident("apple"),
                    Token::punc(".", '.'),
                    Token::ident("orange"),
                    Token::punc(".", '.'),
                    Token::ident("banana"),
                ][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_command_exclude_trailing_dots() {
            let tokens = lex("apple.orange.").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[
                    Token::ident("apple"),
                    Token::punc(".", '.'),
                    Token::ident("orange"),
                ][..]));
                assert_eq!(args.next(), Some(&[Token::punc(".", '.')][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_command_split_empty_dot() {
            let tokens = lex("apple.orange..banana").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[
                    Token::ident("apple"),
                    Token::punc(".", '.'),
                    Token::ident("orange"),
                ][..]));
                assert_eq!(args.next(), Some(&[Token::punc(".", '.')][..]));
                assert_eq!(args.next(), Some(&[Token::punc(".", '.')][..]));
                assert_eq!(args.next(), Some(&[Token::ident("banana")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_command_split_dotless_ident() {
            let tokens = lex("apple.orange banana").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[
                    Token::ident("apple"),
                    Token::punc(".", '.'),
                    Token::ident("orange"),
                ][..]));
                assert_eq!(args.next(), Some(&[Token::ident("banana")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_struct() {
            let tokens = lex("x:4/y:6/z:1 apple orange").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[
                    Token::ident("x"),
                    Token::punc(":", ':'),
                    Token::number("4"),
                    Token::punc("/", '/'),
                    Token::ident("y"),
                    Token::punc(":", ':'),
                    Token::number("6"),
                    Token::punc("/", '/'),
                    Token::ident("z"),
                    Token::punc(":", ':'),
                    Token::number("1"),
                ][..]));
                assert_eq!(args.next(), Some(&[Token::ident("apple")][..]));
                assert_eq!(args.next(), Some(&[Token::ident("orange")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_struct_keep_trailing() {
            let tokens = lex("x:4/y:6/ apple orange").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[
                    Token::ident("x"),
                    Token::punc(":", ':'),
                    Token::number("4"),
                    Token::punc("/", '/'),
                    Token::ident("y"),
                    Token::punc(":", ':'),
                    Token::number("6"),
                    Token::punc("/", '/'),
                    Token::ident("apple"),
                ][..]));
                assert_eq!(args.next(), Some(&[Token::ident("orange")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_struct_keep_trailing_field() {
            let tokens = lex("x:4/y: apple orange").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[
                    Token::ident("x"),
                    Token::punc(":", ':'),
                    Token::number("4"),
                    Token::punc("/", '/'),
                    Token::ident("y"),
                    Token::punc(":", ':'),
                    Token::ident("apple"),
                ][..]));
                assert_eq!(args.next(), Some(&[Token::ident("orange")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_group_rgb() {
            let tokens = lex("rgb(43, 22, 54) apple").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(args), {
                let mut args = args.split_args();
                assert_eq!(args.next(), Some(&[
                    Token::ident("rgb"),
                    Token::punc("(", '('),
                    Token::number("43"),
                    Token::punc(",", ','),
                    Token::number("22"),
                    Token::punc(",", ','),
                    Token::number("54"),
                    Token::punc(")", ')'),
                ][..]));
                assert_eq!(args.next(), Some(&[Token::ident("apple")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }
    }
}
