use std::str::pattern::{Pattern, SearchStep, Searcher};

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

#[derive(Debug, PartialEq)]
pub enum Pipeline<'a, 'b> {
    Separator(&'b Token<'a>),
    Item(SplitArgs<'a, 'b>),
}

pub fn syntax<'a, 'b>(tokens: &'b [Token<'a>]) -> impl Iterator<Item = Pipeline<'a, 'b>> {
    tokens
        .split_inclusive(|tkn| tkn.eq_punc('|'))
        .flat_map(|mut tokens| {
            let sep = tokens
                .last().is_some_and(|tkn| tkn.eq_punc('|'))
                .then(|| Pipeline::Separator(tokens.split_off_last().unwrap()));

            let item = Some(Pipeline::Item(SplitArgs::new(tokens)));

            [item, sep].into_iter().filter_map(|x| x)
        })
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
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
                assert_eq!(args.next(), Some(&[Token::ident("apple")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_pipeline() {
            let tokens = lex("apple | orange").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
                assert_eq!(args.next(), Some(&[Token::ident("apple")][..]));
                assert_eq!(args.next(), None);
            });
            assert_eq!(it.next(), Some(Pipeline::Separator(const { &Token::punc("|", '|') })));
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
                assert_eq!(args.next(), Some(&[Token::ident("orange")][..]));
                assert_eq!(args.next(), None);
            });
            assert!(it.next().is_none());
        }

        #[test]
        fn test_command_with_dots() {
            let tokens = lex("apple.orange.banana").collect::<Vec<Token>>();
            let mut it = syntax(&tokens);
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
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
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
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
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
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
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
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
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
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
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
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
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
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
            assert_syntax!(it.next(), Some(Pipeline::Item(mut args)), {
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
