use super::{lex::*, Cmd, CmdError};

#[derive(Debug, PartialEq)]
pub struct SubRoutine<'a> {
    cmd: Cmd,
    args: Vec<Box<AdjTokens<'a>>>,
}

#[derive(Debug, PartialEq)]
pub struct Routine<'a> {
    lines: Vec<SubRoutine<'a>>,
}

#[derive(Debug)]
pub enum CompileErrorType {
    MissingCmd,
    CmdError(CmdError),
}

#[derive(Debug)]
pub struct CompileError {
    kind: CompileErrorType,
    code: String,
    line: usize,
    cols: std::ops::Range<usize>,
}

impl CompileError {
    pub fn new(kind: CompileErrorType, src: &str, substr: &str) -> Self {
        let cols = src.substr_range(substr).expect("substr should be within src");
        let line = src[..cols.start].matches('|').count() + 1;
        Self {
            kind,
            code: substr.to_string(),
            line,
            cols,
        }
    }

    #[inline]
    pub fn missing_cmd(src: &str, substr: &str) -> Self {
        Self::new(CompileErrorType::MissingCmd, src, substr)
    }

    #[inline]
    pub fn cmd_error(e: CmdError, src: &str, substr: &str) -> Self {
        Self::new(CompileErrorType::CmdError(e), src, substr)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "item {}, columns {}-{}: ", self.line, self.cols.start, self.cols.end)?;
        match self.kind {
            CompileErrorType::MissingCmd => "line has no command".fmt(f)?,
            CompileErrorType::CmdError(_) => "command error".fmt(f)?,
        }
        write!(f, "  `{}`", self.code)
    }
}

impl std::error::Error for CompileError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            CompileErrorType::MissingCmd => None,
            CompileErrorType::CmdError(e) => Some(e),
        }
    }
}

pub fn compile<'a, 'b>(src: &'a str) -> Result<Routine<'a>, CompileError> {
    let tokens = lex(src).collect::<Vec<Token<'a>>>();
    syntax(&tokens)
        .filter_map(|p| (p != &[Token::punc("|", '|')]).then(|| p.adj_chunks().map(ToOwned::to_owned)))
        .map(|mut it| {
            if let Some(cmd) = it.next() {
                let cmd = cmd.to_str().parse().map_err(|e| CompileError::cmd_error(e, src, todo!()))?;
                let args = it.collect::<Vec<_>>();
                Ok(SubRoutine { cmd, args })
            } else {
                Err(CompileError::missing_cmd(src, todo!()))
            }
        })
        .collect::<Result<Vec<_>, CompileError>>()
        .map(|lines| Routine { lines })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test0() {
        let prgm = compile("sv.route a b c");
        match prgm {
            Ok(prgm) => {
                let mut lines = prgm.lines.iter();
                let line = lines.next().unwrap();
                assert_eq!(line.cmd, Cmd::SvRoute);
                let mut args = line.args.iter().map(|x| x as &[Token]);
                assert_eq!(args.next(), Some(&[Token::ident("a")][..]));
                assert_eq!(args.next(), Some(&[Token::ident("b")][..]));
                assert_eq!(args.next(), Some(&[Token::ident("c")][..]));
            }
            Err(e) => panic!("compile failed: {e}"),
        }
    }

    #[test]
    fn test_pipeline() {
        let prgm = compile("sv.route a b c | color red");
        match prgm {
            Ok(prgm) => {
                let mut lines = prgm.lines.iter();
                let line = lines.next().unwrap();
                assert_eq!(line.cmd, Cmd::SvRoute);
                let mut args = line.args.iter().map(|x| x as &[Token]);
                assert_eq!(args.next(), Some(&[Token::ident("a")][..]));
                assert_eq!(args.next(), Some(&[Token::ident("b")][..]));
                assert_eq!(args.next(), Some(&[Token::ident("c")][..]));
                let line = lines.next().unwrap();
                assert_eq!(line.cmd, Cmd::Color);
                let mut args = line.args.iter().map(|x| x as &[Token]);
                assert_eq!(args.next(), Some(&[Token::ident("red")][..]));
            }
            Err(e) => panic!("compile failed: {e}"),
        }
    }
}
