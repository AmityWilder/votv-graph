use std::borrow::Cow;
use super::{lex::*, types::SizeHint, Cmd, CmdError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangedType<'a> {
    Text(&'a [&'static str]),
    Cmd(&'a [Cmd]),
    Count(&'a [std::ops::Range<usize>]),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralType<'a> {
    Text(&'a str),
    Cmd(Cmd),
    Count(usize),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type<'a> {
    Text,
    Cmd,
    Count,
    Bool,
    Ranged(RangedType<'a>),
    Literal(LiteralType<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeBounds<'a> {
    ty: Type<'a>,
    len: SizeHint,
}

impl<'a> TypeBounds<'a> {
    #[inline]
    pub const fn simple(ty: Type<'a>) -> Self {
        Self::array(ty, SizeHint::new(1))
    }

    pub const fn array(ty: Type<'a>, len: SizeHint) -> Self {
        Self { ty, len }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RetBounds {
    /// Array with ranged len must not be followed by an argument of the same type.
    Always(&'static [TypeBounds<'static>]),
    Mapped(for<'a, 'b> fn(&'a [TypeBounds<'b>]) -> Cow<'a, [TypeBounds<'a>]>),
}

pub trait CoerceType {
    fn coerce<'a>(&'a self, into: &'a Type<'a>) -> Option<Type<'a>> where Self: 'a;
}

impl CoerceType for AdjTokens<'_> {
    fn coerce<'a>(&'a self, into: &'a Type<'a>) -> Option<Type<'a>> where Self: 'a {
        match into {
            Type::Text => Some(*into),
            Type::Cmd => self.to_str().and_then(|s| s.parse().ok().map(|cmd| Type::Literal(LiteralType::Cmd(cmd)))),
            Type::Count => if let [tkn] = &**self && tkn.is_number() { Some(Type::Count) } else { None },
            Type::Bool => {
                if let [tkn] = &**self {
                    if tkn == "true" || tkn == "1" {
                        return Some(Type::Ranged(RangedType::Bool(true)));
                    } else if tkn == "false" || tkn == "0" {
                        return Some(Type::Ranged(RangedType::Bool(false)));
                    }
                }
                None
            },
            Type::Ranged(t) => match t {
                RangedType::Text(opts) => {
                    let s = self.to_str().unwrap_or("");
                    opts.contains(&s)
                        .then(|| Type::Literal(LiteralType::Text(s)))
                },
                RangedType::Cmd(opts) => {
                    self.to_str()
                        .and_then(|s| s.parse().ok())
                        .and_then(|c|
                            opts.contains(&c)
                                .then(|| Type::Literal(LiteralType::Cmd(c)))
                        )
                },
                RangedType::Count(_ranges) => todo!(),
                &RangedType::Bool(b) => if let [tkn] = self as &[Token] {
                    match b {
                        true => matches!(tkn.src, "true"|"1"),
                        false => matches!(tkn.src, "false"|"0"),
                    }.then(|| Type::Literal(LiteralType::Bool(b)))
                } else {
                    None
                },
            },
            Type::Literal(v) => match v {
                LiteralType::Text(v) => self.to_str().unwrap_or("") == *v,
                LiteralType::Cmd(v) => self.to_str().and_then(|s| s.parse().ok()).is_some_and(|c: Cmd| c == *v),
                LiteralType::Count(v) => if let [tkn] = self as &[Token] {
                    tkn.is_number() && tkn.src.parse().is_ok_and(|n: usize| n == *v)
                } else {
                    false
                },
                LiteralType::Bool(v) => if let [tkn] = self as &[Token] {
                    matches!((v, tkn.src), (true, "true"|"1") | (false, "false"|"0"))
                } else {
                    false
                },
            }.then(|| *into)
        }
    }
}

pub struct Signature {
    pub args: &'static [TypeBounds<'static>],
    pub rets: RetBounds,
}

impl Cmd {
    pub const fn signature(&self) -> &'static [Signature] {
        match self {
            Cmd::Help => const { &[
                Signature {
                    args: &[],
                    rets: RetBounds::Always(const { &[TypeBounds::simple(Type::Text)] }),
                },
                Signature {
                    args: &[TypeBounds::simple(Type::Cmd)],
                    rets: RetBounds::Always(const { &[TypeBounds::simple(Type::Text)] }),
                },
            ] }
            Cmd::Close => const { &[
                Signature {
                    args: &[],
                    rets: RetBounds::Always(&[]),
                },
            ] },
            Cmd::Cls => const { &[
                Signature {
                    args: &[],
                    rets: RetBounds::Always(&[]),
                },
            ] },
            Cmd::Echo => const { &[
                Signature {
                    args: &[
                        TypeBounds::simple(Type::Count),
                        TypeBounds::array(Type::Text, SizeHint::new_range(0, None)),
                    ],
                    rets: RetBounds::Mapped(|args| Cow::Borrowed(&args[1..])),
                },
            ] },
            Cmd::Dbg => const { &[
                Signature {
                    args: &[],
                    rets: RetBounds::Always(&[]),
                },
                Signature {
                    args: &[TypeBounds::simple(Type::Bool)],
                    rets: RetBounds::Always(&[]),
                },
            ] },
            Cmd::Focus => todo!(),
            Cmd::Color => todo!(),
            Cmd::SvRoute => todo!(),
            Cmd::SvRouteAdd => todo!(),
            Cmd::SvRouteList => todo!(),
            Cmd::SvRouteClear => todo!(),
            Cmd::SvNew => todo!(),
            Cmd::SvEdge => todo!(),
            Cmd::SvLoad => todo!(),
            Cmd::SvSave => todo!(),
            Cmd::Tempo => todo!(),
            Cmd::Skip => todo!(),
            Cmd::Take => todo!(),
        }
    }

    pub fn pick_signature<'a, I>(&self, args: I) -> Option<&'static Signature>
    where
        I: IntoIterator<
            Item = &'a dyn CoerceType,
            IntoIter: ExactSizeIterator + Clone,
        >,
    {
        let it = args.into_iter().peekable();
        self.signature().iter()
            .find(|sig| {
                let mut args = it.clone();
                for ty in sig.args {
                    let t = &ty.ty;
                    for i in 0..ty.len.high.unwrap_or_else(|| args.len()) {
                        if args.peek().and_then(|x| x.coerce(t)).is_some() {
                            _ = args.next();
                        } else if i >= ty.len.low {
                            break;
                        } else {
                            return false;
                        }
                    }
                }
                args.next().is_none() // all must be consumed
            })
    }
}

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
                let cmd = cmd.to_str().unwrap_or("").parse().map_err(|e| CompileError::cmd_error(e, src, todo!()))?;
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
}
