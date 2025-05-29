use super::{lex::{adjacent::{AdjChunks, AdjTokens, TokenSliceExt}, *}, types::{signature::{Argument, Signature}, TypeBounds}, Cmd, CmdError};

#[derive(Debug, PartialEq)]
pub struct SubRoutine<'a> {
    cmd: Cmd,
    sig: &'static Signature,
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
    TypeMismatch,
}

#[derive(Debug)]
pub struct CompileError {
    kind: CompileErrorType,
    // code: String,
    // line: usize,
    // cols: std::ops::Range<usize>,
}

impl CompileError {
    pub fn new(kind: CompileErrorType, src: &str) -> Self {
        // let cols = src.substr_range(substr).expect("substr should be within src");
        // let line = src[..cols.start].matches('|').count() + 1;
        Self {
            kind,
            // code: substr.to_string(),
            // line,
            // cols,
        }
    }

    #[inline]
    pub fn missing_cmd(src: &str) -> Self {
        Self::new(CompileErrorType::MissingCmd, src)
    }

    #[inline]
    pub fn cmd_error(e: CmdError, src: &str) -> Self {
        Self::new(CompileErrorType::CmdError(e), src)
    }

    #[inline]
    pub fn type_mismatch(src: &str) -> Self {
        Self::new(CompileErrorType::TypeMismatch, src)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "item {}, columns {}-{}: ", self.line, self.cols.start, self.cols.end)?;
        match self.kind {
            CompileErrorType::MissingCmd => "line has no command".fmt(f),
            CompileErrorType::CmdError(_) => "command error".fmt(f),
            CompileErrorType::TypeMismatch => "type mismatch".fmt(f),
        }
        // write!(f, "  `{}`", self.code)
    }
}

impl std::error::Error for CompileError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            CompileErrorType::MissingCmd => None,
            CompileErrorType::CmdError(e) => Some(e),
            CompileErrorType::TypeMismatch => None,
        }
    }
}

#[derive(Debug)]
pub struct InternalItemModel<'a, 'b: 'a> {
    pub signature: (Cmd, &'static Signature),
    pub given_args: Vec<&'a AdjTokens<'b>>,
    pub coerced_types: Vec<(bool, TypeBounds)>,
}

#[derive(Debug)]
pub struct InternalModel<'a, 'b: 'a> {
    pub lines: Vec<InternalItemModel<'a, 'b>>,
}

pub fn compile<'a, 'b: 'a>(src: &'b str) -> Result<Routine<'b>, CompileError> {
    let tokens = lex(src).collect::<Vec<Token<'b>>>();
    let mut ret_types: Vec<TypeBounds> = Vec::new();
    let mut it = syntax(&tokens)
        .filter_map(|p|
            // Right now there is no purpose to inspecting the separators.
            // In the future, there might be different ones that modify execution, however.
            if p != &[Token::punc("|", '|')] { Some(p.adj_chunks()) } else { None }
        );

    let internal_model = Vec::new();

    // for  {
    //     internal_model.push(value);
    // }

    //     .map(|mut it| {
    //         if let Some(cmd) = it.next() {
    //             let cmd: Cmd = cmd.to_str()
    //                 .parse()
    //                 .map_err(|e| CompileError::cmd_error(e, src))?;

    //             let given_args = it.map(ToOwned::to_owned)
    //                 .collect::<Vec<Box<AdjTokens<'b>>>>();

    //             let sig = cmd.signature()
    //                 .select(
    //                     given_args.iter().map(|x| &**x).map(Argument::Given)
    //                         .chain(ret_types.iter().map(Argument::Piped))
    //                 );

    //             if let Some((sig, (is_guaranteed, ts))) = sig {

    //                 ret_types = ts;

    //                 Ok(SubRoutine { cmd, sig, args: given_args })
    //             } else {
    //                 Err(CompileError::type_mismatch(src))
    //             }
    //         } else {
    //             Err(CompileError::missing_cmd(src))
    //         }
    //     })
    //     .collect::<Result<Vec<_>, CompileError>>()
    //     .map(|lines| Routine { lines })

    todo!()
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
