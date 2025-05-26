use crate::lexer::Token;

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
    Sequence {
        items: Vec<SyntaxNode<'a>>,
        separators: Vec<Token<'a>>,
    },
    Struct {
        items: Vec<Field<'a>>,
        separators: Vec<Token<'a>>,
    },
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
    todo!()
}
