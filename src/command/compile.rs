use super::lex::*;

#[derive(Debug, Clone)]
pub struct Routine<'a> {
    lines: Vec<Token<'a>>,
}

pub fn compile<'a, 'b>(syn: Syntax<'a, 'b>) -> Routine<'a> {
    todo!()
}
