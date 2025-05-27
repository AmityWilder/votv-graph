use std::borrow::Cow;
use crate::command::{lex::AdjTokens, Cmd};
use super::{CoerceArg, Coercion, RetBounds, SizeHint, Type, TypeBounds};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Signature {
    pub args: &'static [TypeBounds],
    pub rets: RetBounds,
}

const CMD_HELP_SIG: [Signature; 2] = [
    Signature {
        args: &[],
        rets: RetBounds::pure(&[TypeBounds::simple(Type::Text)]),
    },
    Signature {
        args: &[TypeBounds::simple(Type::Cmd)],
        rets: RetBounds::pure(&[TypeBounds::simple(Type::Text)]),
    },
];

const CMD_VOID_TO_VOID_SIG: [Signature; 1] = [
    Signature {
        args: &[],
        rets: RetBounds::pure(&[]),
    },
];

const CMD_ECHO_SIG: [Signature; 1] = [
    Signature {
        args: &[TypeBounds::array(Type::Any, SizeHint::new_range(0, None))],
        rets: RetBounds::lambda(
            &[TypeBounds::array(Type::Any, SizeHint::new_range(0, None))],
            |args| args.into_iter().map(|t| TypeBounds::simple(*t)).collect(),
        ),
    },
];

const CMD_DBG_SIG: [Signature; 2] = [
    Signature {
        args: &[],
        rets: RetBounds::pure(&[]),
    },
    Signature {
        args: &[TypeBounds::simple(Type::Bool)],
        rets: RetBounds::pure(&[]),
    },
];

const CMD_ROUTE_SIG: [Signature; 1] = [
    Signature {
        args: &[TypeBounds::array(Type::Text, SizeHint::new_range(2, None))],
        rets: RetBounds::lambda(
            &[TypeBounds::array(Type::Text, SizeHint::new_range(2, None))],
            |args| [TypeBounds::array(Type::Text, SizeHint::new_range(args.len(), None))].to_vec(),
        ),
    },
];

const CMD_ROUTE_LIST_SIG: [Signature; 1] = [
    Signature {
        args: &[],
        rets: RetBounds::pure(&[TypeBounds::array(Type::Text, SizeHint::new_range(0, None))]),
    },
];

const CMD_TEMPO_SIG: [Signature; 2] = [
    Signature {
        args: &[TypeBounds::simple(Type::Tempo)],
        rets: RetBounds::pure(&[]),
    },
    Signature {
        args: &[],
        rets: RetBounds::pure(&[TypeBounds::simple(Type::Tempo)]),
    },
];

const CMD_SKIP_SIG: [Signature; 1] = [
    Signature {
        args: &[
            TypeBounds::simple(Type::Count),
            TypeBounds::array(Type::Any, SizeHint::new_range(0, None)),
        ],
        rets: RetBounds::lambda(
            &[TypeBounds::array(Type::Any, SizeHint::new_range(0, None))],
            |args| {
                match args {
                    [Type::Count, rest @ ..] =>
                        rest.chunk_by(|a, b| a == b)
                            .map(|chunk| TypeBounds::array(chunk[0], SizeHint::new_range(0, Some(chunk.len()))))
                            .collect(),

                    [Type::RangedCount(_opts), _rest @ ..] => todo!("waiting to see if this ever comes up"),

                    [Type::ConstCount(n), rest @ ..] => {
                        let mut n = *n;
                        rest.chunk_by(|a, b| a == b)
                            .filter_map(|chunk| {
                                let kept = chunk.len().saturating_sub(n);
                                n = n.saturating_sub(kept);
                                if kept > 0 {
                                    Some(TypeBounds::array(chunk[0], SizeHint::new(kept)))
                                } else {
                                    None
                                }
                            })
                            .collect()
                    }

                    _ => panic!("incorrect use of RetBounds"),
                }
            },
        ),
    },
];

const CMD_TAKE_SIG: [Signature; 1] = [
    Signature {
        args: &[
            TypeBounds::simple(Type::Count),
            TypeBounds::array(Type::Any, SizeHint::new_range(0, None)),
        ],
        rets: RetBounds::lambda(
            &[TypeBounds::array(Type::Any, SizeHint::new_range(0, None))],
            |args| {
                match args {
                    [Type::Count, rest @ ..] =>
                        rest.chunk_by(|a, b| a == b)
                            .map(|chunk| TypeBounds::array(chunk[0], SizeHint::new_range(0, Some(chunk.len()))))
                            .collect(),

                    [Type::RangedCount(_opts), _rest @ ..] => todo!("waiting to see if this ever comes up"),

                    [Type::ConstCount(n), rest @ ..] => {
                        let mut n = *n;
                        rest.chunk_by(|a, b| a == b)
                            .filter_map(|chunk| {
                                let kept = chunk.len().min(n);
                                n = n.saturating_sub(kept);
                                if kept > 0 {
                                    Some(TypeBounds::array(chunk[0], SizeHint::new(kept)))
                                } else {
                                    None
                                }
                            })
                            .fuse()
                            .collect()
                    }

                    _ => panic!("incorrect use of RetBounds"),
                }
            },
        ),
    },
];

impl Cmd {
    pub const fn signature(&self) -> &'static SignatureList {
        match self {
            Cmd::Help => SignatureList::new(&CMD_HELP_SIG),
            Cmd::Close | Cmd::Cls | Cmd::SvRouteClear => SignatureList::new(&CMD_VOID_TO_VOID_SIG),
            Cmd::Echo => SignatureList::new(&CMD_ECHO_SIG),
            Cmd::Dbg => SignatureList::new(&CMD_DBG_SIG),
            Cmd::Focus => todo!(),
            Cmd::Color => todo!(),
            Cmd::SvRoute | Cmd::SvRouteAdd => SignatureList::new(&CMD_ROUTE_SIG),
            Cmd::SvRouteList => SignatureList::new(&CMD_ROUTE_LIST_SIG),
            Cmd::SvNew => todo!(),
            Cmd::SvEdge => todo!(),
            Cmd::SvLoad => todo!(),
            Cmd::SvSave => todo!(),
            Cmd::Tempo => SignatureList::new(&CMD_TEMPO_SIG),
            Cmd::Skip => SignatureList::new(&CMD_SKIP_SIG),
            Cmd::Take => SignatureList::new(&CMD_TAKE_SIG),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SignatureList([Signature]);

impl std::ops::Deref for SignatureList {
    type Target = [Signature];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<[Signature]> for &SignatureList {
    #[inline]
    fn as_ref(&self) -> &[Signature] {
        &self.0
    }
}

impl Signature {
    pub fn coerce_sig<'a, 'b: 'a, I>(&self, args: I) -> Coercion<Vec<Type>>
    where
        I: Iterator<Item = Argument<'a, 'b>>,
    {
        use Coercion::*;

        let mut is_always = true;
        let mut result = Vec::with_capacity(self.args.len());

        let mut args = args.peekable();
        'signature: for ty in self.args {
            let t = &ty.ty;
            'argument: for i in 0..ty.len.high.unwrap_or(isize::MAX as usize) {
                let next = args.peek()
                    .and_then(|x| x.coerce_arg(t).into_checked());

                if let Some((is_certain, item)) = next {
                    _ = args.next();
                    if !is_certain {
                        is_always = false;
                    }
                    result.push(item);
                } else {
                    if i >= ty.len.low {
                        break 'argument;
                    } else {
                        break 'signature;
                    }
                }
            }
        }

        if args.next().is_none() {
            if is_always { Always(result) } else { Maybe(result) }
        } else {
            // all must be consumed
            Never
        }
    }
}

impl SignatureList {
    pub const fn new(arr: &[Signature]) -> &Self {
        unsafe { &*(arr as *const [Signature] as *const Self) }
    }

    #[inline]
    pub fn select<'a, 'b: 'a, I>(&self, args: I) -> Option<(&Signature, (bool, Vec<Type>))>
    where
        I: IntoIterator<
            Item = Argument<'a, 'b>,
            IntoIter: Iterator + Clone,
        >,
    {
        let it = args.into_iter();
        self.iter()
            .find_map(|sig| {
                sig
                    .coerce_sig(it.clone())
                    .into_checked()
                    .map(|x| (sig, x))
            })
    }
}

#[derive(Debug, Clone)]
pub enum Argument<'a, 'b: 'a> {
    Given(&'a AdjTokens<'b>),
    Piped(Type),
}

impl CoerceArg for Argument<'_, '_> {
    fn coerce_arg(&self, into: &Type) -> Coercion<Type> {
        match self {
            Self::Given(x) => x.coerce_arg(into),
            Self::Piped(x) => x.coerce_arg(into),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::command::lex::{lex, Token, TokenSliceExt};
    use super::*;

    #[test]
    fn test0() {
        assert_eq!(Cmd::Help.signature().select([]), Some((&CMD_HELP_SIG[0], (true, [].to_vec()))));
    }

    #[test]
    fn test1() {
        let tokens = lex("sv.route").collect::<Vec<Token>>();
        let it = tokens.adj_chunks().map(|x| Argument::Given(x));
        assert_eq!(Cmd::Help.signature().select(it), Some((&CMD_HELP_SIG[1], (true, [Type::ConstCmd(Cmd::SvRoute)].to_vec()))));
    }

    #[test]
    fn test_coerce_any() {
        let tokens = lex("apple \"banana orange\" 52 * squeak").collect::<Vec<Token>>();
        let it = tokens.adj_chunks().map(|x| Argument::Given(x));
        assert_eq!(Cmd::Echo.signature().select(it), Some((&CMD_ECHO_SIG[0], (true, [
            Type::Text,
            Type::Text,
            Type::ConstCount(52),
            Type::Text,
            Type::Text,
        ].to_vec()))));
    }

    #[test]
    fn test_pipeline() {
        let tokens = lex("3 a b c d e f").collect::<Vec<Token>>();
        let it = tokens.adj_chunks().map(|x| Argument::Given(x));
        let sig = Cmd::Take.signature().select(it);
        assert_eq!(sig, Some((&CMD_TAKE_SIG[0], (true, [
            Type::ConstCount(3),
            Type::Text,
            Type::Text,
            Type::Text,
            Type::Text,
            Type::Text,
            Type::Text,
        ].to_vec()))));
        let (sig, (_, args)) = sig.unwrap();
        println!("{:#?}", sig.rets.apply(&args));
    }
}