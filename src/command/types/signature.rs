use std::borrow::Cow;
use crate::command::{lex::AdjTokens, Cmd};
use super::{CoerceArg, Coercion, Const, Ranged, RetBounds, SizeHint, Type, TypeBounds};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Signature {
    pub args: &'static [TypeBounds<'static>],
    pub rets: RetBounds,
}

const CMD_HELP_SIG: [Signature; 2] = [
    Signature {
        args: &[],
        rets: RetBounds::Always(&[TypeBounds::simple(Type::Text)]),
    },
    Signature {
        args: &[TypeBounds::simple(Type::Cmd)],
        rets: RetBounds::Always(&[TypeBounds::simple(Type::Text)]),
    },
];

const CMD_VOID_TO_VOID_SIG: [Signature; 1] = [
    Signature {
        args: &[],
        rets: RetBounds::void(),
    },
];

const CMD_ECHO_SIG: [Signature; 1] = [
    Signature {
        args: &[TypeBounds::array(Type::Any, SizeHint::new_range(0, None))],
        rets: RetBounds::Mapped(|args| Cow::Borrowed(args)),
    },
];

const CMD_DBG_SIG: [Signature; 2] = [
    Signature {
        args: &[],
        rets: RetBounds::void(),
    },
    Signature {
        args: &[TypeBounds::simple(Type::Bool)],
        rets: RetBounds::void(),
    },
];

const CMD_ROUTE_SIG: [Signature; 1] = [
    Signature {
        args: &[
            TypeBounds::array(Type::Text, SizeHint::new_range(2, None)),
        ],
        rets: RetBounds::Mapped(|args| {
            Cow::Owned([TypeBounds::array(Type::Text, SizeHint::new_range(args[0].len.low, None))][..].to_owned())
        }),
    },
];

const CMD_ROUTE_LIST_SIG: [Signature; 1] = [
    Signature {
        args: &[],
        rets: RetBounds::Always(&[TypeBounds::array(Type::Text, SizeHint::new_range(0, None))]),
    },
];

const CMD_TEMPO_SIG: [Signature; 2] = [
    Signature {
        args: &[TypeBounds::simple(Type::Tempo)],
        rets: RetBounds::void(),
    },
    Signature {
        args: &[],
        rets: RetBounds::Always(&[TypeBounds::simple(Type::Tempo)]),
    },
];

const CMD_SKIP_SIG: [Signature; 1] = [
    Signature {
        args: &[
            TypeBounds::simple(Type::Count),
            TypeBounds::array(Type::Any, SizeHint::new_range(0, None)),
        ],
        rets: RetBounds::Mapped(|args| {
            Cow::Owned([match args {
                [
                    TypeBounds { ty: Type::Count, .. },
                    TypeBounds { ty, len: SizeHint { low: _, high } },
                ] => TypeBounds::array(*ty, SizeHint { low: 0, high: *high }),

                [
                    TypeBounds { ty: Type::Ranged(Ranged::Count(opts)), .. },
                    TypeBounds { ty, len: SizeHint { low, high } },
                ] => TypeBounds::array(*ty, SizeHint {
                    low: low.saturating_sub(opts.max()),
                    high: high.map(|high| high.saturating_sub(opts.min()))
                }),

                [
                    TypeBounds { ty: Type::Const(Const::Count(n)), .. },
                    TypeBounds { ty, len: SizeHint { low, high } },
                ] => TypeBounds::array(*ty, SizeHint {
                    low: low.saturating_sub(*n),
                    high: high.map(|high| high.saturating_sub(*n)),
                }),

                _ => unimplemented!("invalid use of RetBounds"),
            }][..].to_owned())
        }),
    },
];

const CMD_TAKE_SIG: [Signature; 1] = [
    Signature {
        args: &[
            TypeBounds::simple(Type::Count),
            TypeBounds::array(Type::Any, SizeHint::new_range(0, None)),
        ],
        rets: RetBounds::Mapped(|args| {
            Cow::Owned([match args {
                [
                    TypeBounds { ty: Type::Count, .. },
                    TypeBounds { ty, len: SizeHint { low: _, high } },
                ] => TypeBounds::array(*ty, SizeHint { low: 0, high: *high }),

                [
                    TypeBounds { ty: Type::Ranged(Ranged::Count(opts)), .. },
                    TypeBounds { ty, len: SizeHint { low, high } },
                ] => TypeBounds::array(*ty, SizeHint {
                    low: (*low).min(opts.min()),
                    high: {
                        let max = opts.max();
                        Some(high.map_or(max, |high| high.min(max)))
                    },
                }),

                [
                    TypeBounds { ty: Type::Const(Const::Count(n)), .. },
                    TypeBounds { ty, len: SizeHint { low, high } },
                ] => TypeBounds::array(*ty, SizeHint {
                    low: (*low).min(*n),
                    high: Some(high.map_or(*n, |high| high.min(*n))),
                }),

                _ => unimplemented!("invalid use of RetBounds"),
            }][..].to_owned())
        }),
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

impl SignatureList {
    pub const fn new(arr: &[Signature]) -> &Self {
        unsafe { &*(arr as *const [Signature] as *const Self) }
    }

    pub fn select<'a, 'b: 'a, I>(&self, args: I) -> Option<&Signature>
    where
        I: IntoIterator<
            Item = Argument<'a, 'b>,
            IntoIter: Iterator + Clone,
        >,
    {
        let it = args.into_iter().peekable();
        self.iter()
            .find(|sig| {
                let mut args = it.clone();
                for ty in sig.args {
                    let t = &ty.ty;
                    for i in 0..ty.len.high.unwrap_or(isize::MAX as usize) {
                        if args.peek().is_some_and(|x| x.coerce_arg(t).is_possible()) {
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

#[derive(Debug, Clone)]
pub enum Argument<'a, 'b> {
    Given(&'a AdjTokens<'b>),
    Piped(Type<'a>),
}

impl CoerceArg for Argument<'_, '_> {
    fn coerce_arg<'a>(&'a self, into: &'a Type<'a>) -> Coercion<Type<'a>> where Self: 'a {
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
        assert_eq!(Cmd::Help.signature().select([]), Some(&CMD_HELP_SIG[0]));
    }

    #[test]
    fn test1() {
        let tokens = lex("sv.route").collect::<Vec<Token>>();
        let it = tokens.adj_chunks().map(|x| Argument::Given(x));
        assert_eq!(Cmd::Help.signature().select(it), Some(&CMD_HELP_SIG[1]));
    }

    #[test]
    fn test_pipe() {
        let tokens = lex("apple \"banana orange\" 52 * squeak").collect::<Vec<Token>>();
        let it = tokens.adj_chunks().map(|x| Argument::Given(x));
        assert_eq!(Cmd::Echo.signature().select(it), Some(&CMD_ECHO_SIG[0]));
    }
}