use std::borrow::Cow;

use crate::types::{Coords, RichColor, Tempo};
use super::{lex::{AdjTokens, Token}, Cmd};

pub mod signature;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct SizeHint {
    pub low: usize,
    pub high: Option<usize>,
}

impl std::fmt::Display for SizeHint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let low = self.low;
        match self.high {
            Some(high) => if low == high {
                low.fmt(f)
            } else {
                write!(f, "{low}..={high}")
            },
            None => write!(f, "{low}.."),
        }
    }
}

impl SizeHint {
    #[inline]
    pub const fn new(n: usize) -> Self {
        Self::new_range(n, Some(n))
    }

    #[inline]
    pub const fn new_range(low: usize, high: Option<usize>) -> Self {
        Self { low, high }
    }

    #[inline]
    pub const fn is_exactly(&self, n: usize) -> bool {
        match self.high {
            None => false,
            Some(high) => self.low == high && high == n
        }
    }
}

impl SizeHint {
    #[inline]
    pub const fn contains(&self, n: usize) -> bool {
        self.low <= n && match self.high {
            Some(high) => n <= high,
            None => true,
        }
    }

    #[inline]
    pub const fn contains_range(&self, other: &Self) -> bool {
        self.contains(other.low) && match other.high {
            Some(high) => self.contains(high),
            None => self.high.is_none(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    /// No restriction
    Any,

    /// Any string
    Text,
    /// Any identifier (`[a-zA-Z_][a-zA-Z0-9_]*`)
    Ident,
    /// Any command
    Cmd,
    /// Any unsigned integer
    Count,
    /// Either `true` or `false`
    Bool,
    /// Any RGBA color
    Color,
    /// Any position
    Coords,
    /// Any tempo
    Tempo,

    /// Any one of the specified strings
    RangedText(&'static [&'static str]),
    /// Any one of the specified identifiers
    RangedIdent(&'static [&'static str]),
    /// Any one of the specified commands
    RangedCmd(&'static [Cmd]),
    /// Any unsigned integer in the range
    RangedCount(SizeHint),

    /// A specific string
    ConstText(&'static str),
    /// A specific identifier
    ConstIdent(&'static str),
    /// A specific command
    ConstCmd(Cmd),
    /// A specific unsigned integer
    ConstCount(usize),
    /// A specific boolean
    ConstBool(bool),
    /// A specific tempo (keyword)
    ConstTempo(Tempo),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => "any".fmt(f),
            Self::Text => "str".fmt(f),
            Self::Ident => "ident".fmt(f),
            Self::Cmd => "cmd".fmt(f),
            Self::Count => "uint".fmt(f),
            Self::Bool => "bool".fmt(f),
            Self::Color => "color".fmt(f),
            Self::Coords => "vec3".fmt(f),
            Self::Tempo => "tempo".fmt(f),
            Self::RangedText (opts) => opts.iter().map(|s| format!("\"{s}\"")) .collect::<Vec<_>>().join(" | ").fmt(f),
            Self::RangedIdent(opts) => opts.iter().map(|s| format!( "`{s}`" )) .collect::<Vec<_>>().join(" | ").fmt(f),
            Self::RangedCmd  (opts) => opts.iter().map(|c| format!("cmd::{c}")).collect::<Vec<_>>().join(" | ").fmt(f),
            Self::RangedCount(range) => range.fmt(f),
            Self::ConstText (val) => write!(f, "\"{val}\""),
            Self::ConstIdent(val) => write!(f, "`{val}`"),
            Self::ConstCmd  (val) => write!(f, "cmd::{val}"),
            Self::ConstCount(val) => val.fmt(f),
            Self::ConstBool (val) => val.fmt(f),
            Self::ConstTempo(Tempo::Sync) => "tempo::sync".fmt(f),
            Self::ConstTempo(Tempo::Sprint) => "tempo::sprint".fmt(f),
            Self::ConstTempo(Tempo::Instant) => "tempo::instant".fmt(f),
            Self::ConstTempo(Tempo::Pause) => "tempo::pause".fmt(f),
            Self::ConstTempo(Tempo::Exact { .. }) => panic!("exact tempo not intended for constants"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeBounds {
    pub ty: Type,
    pub len: SizeHint,
}

impl TypeBounds {
    #[inline]
    pub const fn simple(ty: Type) -> Self {
        Self::array(ty, SizeHint::new(1))
    }

    pub const fn array(ty: Type, len: SizeHint) -> Self {
        Self { ty, len }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RetBounds {
    /// The general return signature, used as a default and for un-called metadata.
    ///
    /// Array with ranged len must not be followed by an argument of the same type.
    pub def: &'static [TypeBounds],

    /// Monomorphizes return type given the call signature.
    /// Assumes the types are compatible with the arguments, and may panic if they are not.
    ///
    /// This field is set to [`None`] if the function is already monomorphic.
    pub lambda: Option<fn(args: &[Type]) -> Vec<TypeBounds>>,
}

impl RetBounds {
    #[inline]
    pub const fn pure(def: &'static [TypeBounds]) -> Self {
        Self { def, lambda: None }
    }

    #[inline]
    pub const fn lambda(def: &'static [TypeBounds], f: fn(args: &[Type]) -> Vec<TypeBounds>) -> Self {
        Self { def, lambda: Some(f) }
    }

    #[inline]
    pub fn apply(&self, args: &[Type]) -> Cow<'static, [TypeBounds]> {
        match self.lambda {
            None => Cow::Borrowed(self.def),
            Some(f) => Cow::Owned(f(args)),
        }
    }
}

pub trait CoerceArg {
    /// Converts to the most specific subset of `into` that `self` is guaranteed to belong to.
    fn coerce_arg(&self, into: &Type) -> Option<(bool, Type)>;
}

#[inline]
fn coerse_text(tokens: &AdjTokens<'_>) -> Option<(bool, Type)> {
    if let [tkn] = tokens as &[Token] {
        if tkn.is_str() {
            return Some((true, Type::Text));
        }
    }
    None
}

#[inline]
fn coerse_ident(tokens: &AdjTokens<'_>) -> Option<(bool, Type)> {
    if let [tkn] = tokens as &[Token] {
        if tkn.is_ident() {
            return Some((true, Type::Ident));
        }
    }
    None
}

#[inline]
fn coerse_count(tokens: &AdjTokens<'_>) -> Option<(bool, Type)> {
    if let [tkn] = tokens as &[Token] {
        if tkn.is_number() {
            return Some((true, if let Ok(n) = tkn.src.parse() {
                Type::ConstCount(n)
            } else {
                Type::Count
            }));
        }
    }
    None
}

#[inline]
fn coerce_from_str<T, F>(tokens: &AdjTokens<'_>, f: F) -> Option<(bool, Type)>
where
    T: std::str::FromStr,
    F: FnOnce(T) -> Type,
{
    tokens.to_str().parse().ok().map(|x| (true, f(x)))
}

fn coerce_any(tokens: &AdjTokens<'_>) -> Option<(bool, Type)> {
    if let [tkn] = tokens as &[Token] {
        if tkn.is_number() {
            return Some((true, if let Ok(n) = tkn.src.parse() {
                Type::ConstCount(n)
            } else {
                Type::Count
            }));
        } else if tkn.is_ident() {
            if let Ok(x) = tkn.src.parse() {
                return Some((true, Type::ConstBool(x)));
            }
        }
    }

    let s = tokens.to_str();
    if let Ok(x) = s.parse() {
        return Some((true, Type::ConstCmd(x)));
    } else if let Ok(RichColor(_)) = s.parse() {
        return Some((true, Type::Color));
    } else if let Ok(Coords(_)) = s.parse() {
        return Some((true, Type::Coords));
    } else if let Some(x) = s.parse().ok().filter(|tempo| !matches!(tempo, Tempo::Exact { .. })) {
        return Some((true, Type::ConstTempo(x)));
    } else if let [tkn] = tokens as &[Token] {
        if tkn.is_ident() {
            return Some((true, Type::Ident));
        } else if tkn.is_text() {
            return Some((true, Type::Text));
        }
    }

    Some((true, Type::Any))
}

impl CoerceArg for AdjTokens<'_> {
    fn coerce_arg(&self, into: &Type) -> Option<(bool, Type)> {
        match into {
            Type::Any => coerce_any(self),

            Type::Text => coerse_text(self),
            Type::Ident => coerse_ident(self),
            Type::Cmd => coerce_from_str(self, |x| Type::ConstCmd(x)),
            Type::Count => coerse_count(self),
            Type::Bool => coerce_from_str(self, |b| Type::ConstBool(b)),
            Type::Color => coerce_from_str(self, |RichColor(_)| Type::Color),
            Type::Coords => coerce_from_str(self, |Coords(_)| Type::Coords),
            Type::Tempo => coerce_from_str(self, |x| Type::ConstTempo(x)),

            Type::RangedText(opts) => {
                let s = self.to_str();
                opts.iter()
                    .find(|&&opt| opt == s)
                    .map(|&opt| (true, Type::ConstText(opt)))
            },
            Type::RangedIdent(opts) => {
                if let [tkn] = self as &[Token] && tkn.is_ident() {
                    opts.iter()
                        .find(|&&opt| opt == tkn.src)
                        .map(|&opt| (true, Type::ConstText(opt)))
                } else {
                    None
                }
            },
            Type::RangedCmd(opts) => {
                self.to_str()
                    .parse().ok()
                    .filter(|c| opts.contains(&c))
                    .map(|c| (true, Type::ConstCmd(c)))
            },
            Type::RangedCount(_ranges) => todo!(),

            Type::ConstText(v) => matches!(self as &[Token], [tkn] if tkn.is_str() && tkn.src == *v).then(|| (true, *into)),
            Type::ConstIdent(v) => matches!(self as &[Token], [tkn] if tkn.is_ident() && tkn.src == *v).then(|| (true, *into)),
            Type::ConstCmd(v) => self.to_str().parse().ok().is_some_and(|c: Cmd| c == *v).then(|| (true, *into)),
            Type::ConstCount(v) => if let [tkn] = self as &[Token] {
                tkn.is_number() && tkn.src.parse().is_ok_and(|n: usize| n == *v)
            } else {
                false
            }.then(|| (true, *into)),
            Type::ConstBool(v) => if let [tkn] = self as &[Token] {
                matches!((v, tkn.src), (true, "true"|"1") | (false, "false"|"0"))
            } else {
                false
            }.then(|| (true, *into)),
            Type::ConstTempo(v)  => self.to_str().parse().ok().is_some_and(|x: Tempo| &x == v).then(|| (true, *into)),
        }
    }
}

impl CoerceArg for Type {
    fn coerce_arg(&self, into: &Type) -> Option<(bool, Type)> {
        match (into, self) {
            | (Type::Any, _)
            | (Type::Text, Self::Text | Self::RangedText(_) | Self::ConstText(_))
            | (Type::Cmd, Self::Cmd | Self::RangedCmd(_) | Self::ConstCmd(_))
            | (Type::Count, Self::Count | Self::RangedCount(_) | Self::ConstCount(_))
            | (Type::Bool, Self::Bool | Self::ConstBool(_))
            | (Type::Tempo, Self::Tempo | Self::ConstTempo(_))
            | (Type::RangedText(_), Self::Text)
            | (Type::RangedCmd(_), Self::Cmd)
            | (Type::RangedCount(_), Self::Count)
                => Some((true, *self)),

            // todo

            _ => todo!(),
        }
    }
}
