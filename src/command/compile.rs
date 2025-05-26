use std::str::FromStr;
use super::{lex::*, Cmd, CmdError};

#[derive(Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct CmdTokens<'a>(AdjTokens<'a>);

impl<'a> std::ops::Deref for CmdTokens<'a> {
    type Target = AdjTokens<'a>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> CmdTokens<'a> {
    #[inline]
    pub const fn new<'b>(tokens: &'b AdjTokens<'a>) -> &'b Self {
        // Safety: CmdTokens is just a wrapper for AdjTokens
        unsafe { &*(tokens as *const AdjTokens<'a> as *const Self) }
    }

    #[inline]
    pub fn to_cmd(&self) -> Result<Cmd, CmdError> {
        self.0.to_str()
            .ok_or(CmdError::NoSuchCmd(String::new()))
            .and_then(Cmd::from_str)
    }
}

#[derive(Debug)]
pub struct SubRoutine<'a, 'b> {
    cmd: &'b CmdTokens<'a>,
    args: Vec<&'b AdjTokens<'a>>,
}

#[derive(Debug)]
pub struct Routine<'a, 'b> {
    lines: Vec<SubRoutine<'a, 'b>>,
}

pub enum CompileError {
    MissingCmd,
}

pub fn compile<'a, 'b>(syn: Syntax<'a, 'b>) -> Result<Routine<'a, 'b>, CompileError> {
    syn
        .filter_map(|p| (p != &[Token::punc("|", '|')]).then(|| p.adj_chunks()))
        .map(|mut it| {
            if let Some(cmd) = it.next() {
                let cmd = CmdTokens::new(cmd);
                let args = it.collect();
                Ok(SubRoutine { cmd, args })
            } else {
                Err(CompileError::MissingCmd)
            }
        })
        .collect::<Result<Vec<_>, CompileError>>()
        .map(|lines| Routine { lines })
}
