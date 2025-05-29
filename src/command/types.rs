use std::borrow::Cow;
use crate::types::Tempo;
use super::Cmd;

pub mod signature;
pub mod coerce;
mod incredible;

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
        debug_assert!(if let Some(high) = high { low <= high } else { true });
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
            Self::ConstTempo(val) => match val {
                Tempo::Sync    => "tempo::sync",
                Tempo::Sprint  => "tempo::sprint",
                Tempo::Instant => "tempo::instant",
                Tempo::Pause   => "tempo::pause",
                Tempo::Exact { .. } => panic!("exact tempo not intended for constants"),
            }.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeBounds {
    pub ty: Type,
    pub len: SizeHint,
}

impl std::fmt::Display for TypeBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.len == SizeHint::new(1) {
            self.ty.fmt(f)
        } else {
            write!(f, "[{}; {}]", self.ty, self.len)
        }
    }
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_type_fmt() {
        assert_eq!(Type::ConstText("apple").to_string(), "\"apple\"");
        assert_eq!(Type::ConstIdent("apple").to_string(), "`apple`");
        assert_eq!(Type::ConstCmd(Cmd::SvRoute).to_string(), "cmd::sv.route");
        assert_eq!(Type::RangedText(&["apple orange", "banana"]).to_string(), "\"apple orange\" | \"banana\"");
        assert_eq!(Type::RangedIdent(&["apple", "orange"]).to_string(), "`apple` | `orange`");
    }

    #[test]
    fn test_type_bounds_fmt() {
        assert_eq!(TypeBounds::array(Type::Any, SizeHint::new(1)).to_string(), "any");
        assert_eq!(TypeBounds::array(Type::Any, SizeHint::new(2)).to_string(), "[any; 2]");
        assert_eq!(TypeBounds::array(Type::Any, SizeHint::new_range(2, None)).to_string(), "[any; 2..]");
        assert_eq!(TypeBounds::array(Type::Any, SizeHint::new_range(2, Some(5))).to_string(), "[any; 2..=5]");
    }
}
