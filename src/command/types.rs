use std::borrow::Cow;
use raylib::prelude::*;
use crate::types::{Coords, RichColor, Tempo};
use super::{lex::{AdjTokens, Token}, Cmd};

pub mod signature;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl PartialEq<usize> for SizeHint {
    fn eq(&self, other: &usize) -> bool {
        self.is_exactly(*other)
    }
}

impl<T: std::ops::RangeBounds<usize>> From<T> for SizeHint {
    fn from(value: T) -> Self {
        Self {
            low: match value.start_bound().cloned() {
                std::ops::Bound::Included(n) => n,
                std::ops::Bound::Excluded(n) => n + 1,
                std::ops::Bound::Unbounded => 0,
            },
            high: match value.end_bound().cloned() {
                std::ops::Bound::Included(n) => Some(n),
                std::ops::Bound::Excluded(n) => Some(n - 1),
                std::ops::Bound::Unbounded => None,
            },
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

#[derive(Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct UsizeSet([std::ops::RangeInclusive<usize>]);

impl UsizeSet {
    #[inline]
    pub const fn new(inner: &[std::ops::RangeInclusive<usize>]) -> &Self {
        // SAFETY: UsizeSet is a transparent wrapper
        unsafe { &*(inner as *const [std::ops::RangeInclusive<usize>] as *const Self) }
    }

    #[inline]
    pub fn min(&self) -> usize {
        self.0.iter()
            .map(|range| *range.end())
            .max()
            .expect("RangedType must have at least one range")
    }

    #[inline]
    pub fn max(&self) -> usize {
        self.0.iter()
            .map(|range| *range.end())
            .max()
            .expect("RangedType must have at least one range")
    }

    #[inline]
    pub fn contains(&self, n: usize) -> bool {
        self.0.iter()
            .any(|range| range.contains(&n))
    }

    #[inline]
    pub fn is_superset_of(&self, n: &Self) -> bool {
        n.0.iter()
            .all(|sub_range| {
                self.0.iter()
                    .any(|range| range.start() <= sub_range.start() && sub_range.end() <= range.end())
            })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ranged<'a> {
    Text(&'a [&'static str]),
    Cmd(&'a [Cmd]),
    Count(&'a UsizeSet),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Const<'a> {
    Text(&'a str),
    Cmd(Cmd),
    Count(usize),
    Bool(bool),
    Color(Color),
    Coords(Vector3),
    Tempo(Tempo),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type<'a> {
    Any,
    Text,
    Cmd,
    Count,
    Bool,
    Color,
    Coords,
    Tempo,
    Ranged(Ranged<'a>),
    Const(Const<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RetBounds {
    /// Array with ranged len must not be followed by an argument of the same type.
    Always(&'static [TypeBounds<'static>]),
    /// A closure that takes a slice in the same format as the arguments, but with knowledge of what is being provided.
    Mapped(for<'a, 'b> fn(&'a [TypeBounds<'b>]) -> Cow<'a, [TypeBounds<'a>]>),
}

impl RetBounds {
    #[inline]
    pub const fn void() -> Self {
        Self::Always(&[])
    }
}

pub enum Coercion<T> {
    /// Impossible
    Never,
    /// Possible
    Maybe(T),
    /// Guaranteed
    Always(T),
}
use Coercion::*;

impl<T> Coercion<T> {
    #[must_use]
    #[inline]
    pub const fn is_impossible(&self) -> bool {
        matches!(self, Never)
    }

    #[must_use]
    #[inline]
    pub const fn is_possible(&self) -> bool {
        matches!(self, Maybe(_) | Always(_))
    }

    #[must_use]
    #[inline]
    pub const fn is_certain(&self) -> bool {
        matches!(self, Always(_))
    }

    #[must_use]
    #[inline]
    pub const fn is_uncertain(&self) -> bool {
        matches!(self, Never | Maybe(_))
    }
}

pub trait OptCoerceExt: IntoIterator {
    fn always(self) -> Coercion<Self::Item>;
    fn maybe(self) -> Coercion<Self::Item>;
    fn then_always<U, F: FnOnce(Self::Item) -> U>(self, f: F) -> Coercion<U>;
    fn then_maybe<U, F: FnOnce(Self::Item) -> U>(self, f: F) -> Coercion<U>;
}

impl<T> OptCoerceExt for Option<T> {
    #[inline]
    fn always(self) -> Coercion<T> {
        match self {
            Some(x) => Always(x),
            None => Never,
        }
    }

    #[inline]
    fn maybe(self) -> Coercion<T> {
        match self {
            Some(x) => Maybe(x),
            None => Never,
        }
    }

    #[inline]
    fn then_always<U, F: FnOnce(T) -> U>(self, f: F) -> Coercion<U> {
        match self {
            Some(x) => Always(f(x)),
            None => Never,
        }
    }

    #[inline]
    fn then_maybe<U, F: FnOnce(T) -> U>(self, f: F) -> Coercion<U> {
        match self {
            Some(x) => Maybe(f(x)),
            None => Never,
        }
    }
}

pub trait BoolCoerceExt {
    fn then_always<U, F: FnOnce() -> U>(self, f: F) -> Coercion<U>;
    fn then_maybe<U, F: FnOnce() -> U>(self, f: F) -> Coercion<U>;
    fn then_always_some<T>(self, value: T) -> Coercion<T>;
    fn then_maybe_some<T>(self, value: T) -> Coercion<T>;
}

impl BoolCoerceExt for bool {
    #[inline]
    fn then_always<U, F: FnOnce() -> U>(self, f: F) -> Coercion<U> {
        if self { Always(f()) } else { Never }
    }

    #[inline]
    fn then_maybe<U, F: FnOnce() -> U>(self, f: F) -> Coercion<U> {
        if self { Maybe(f()) } else { Never }
    }

    #[inline]
    fn then_always_some<T>(self, value: T) -> Coercion<T> {
        if self { Always(value) } else { Never }
    }

    #[inline]
    fn then_maybe_some<T>(self, value: T) -> Coercion<T> {
        if self { Maybe(value) } else { Never }
    }
}

pub trait ResultCoerceExt {
    type T;
    type E;

    fn then_always_or_maybe<U, F: FnOnce(Self::T) -> U, G: FnOnce(Self::E) -> U>(self, g: G, f: F) -> Coercion<U>;
    fn then_always_or_else<U, F: FnOnce(Self::T) -> U, G: FnOnce(Self::E) -> Coercion<U>>(self, g: G, f: F) -> Coercion<U>;
}

impl<T, E> ResultCoerceExt for Result<T, E> {
    type T = T;
    type E = E;

    #[inline]
    fn then_always_or_maybe<U, F: FnOnce(T) -> U, G: FnOnce(E) -> U>(self, g: G, f: F) -> Coercion<U> {
        match self {
            Ok(t) => Always(f(t)),
            Err(e) => Maybe(g(e)),
        }
    }

    #[inline]
    fn then_always_or_else<U, F: FnOnce(T) -> U, G: FnOnce(E) -> Coercion<U>>(self, g: G, f: F) -> Coercion<U> {
        match self {
            Ok(t) => Always(f(t)),
            Err(e) => g(e),
        }
    }
}

pub trait CoerceArg {
    /// Converts to the most specific subset of `into` that `self` is guaranteed to belong to.
    fn coerce_arg<'a>(&'a self, into: &'a Type<'a>) -> Coercion<Type<'a>> where Self: 'a;
}

impl CoerceArg for AdjTokens<'_> {
    fn coerce_arg<'a>(&'a self, into: &'a Type<'a>) -> Coercion<Type<'a>> where Self: 'a {
        use Coercion::*;

        match into {
            Type::Any => Always(Type::Any),
            Type::Text => if let [tkn] = self as &[Token] && tkn.is_str() { Always(Type::Const(Const::Text(tkn.src))) } else { Never },
            Type::Cmd => self.to_str().parse().ok().then_always(|x| Type::Const(Const::Cmd(x))),
            Type::Count => if let [tkn] = self as &[Token] && tkn.is_number() { Always(tkn.src.parse().map_or(Type::Count, |n| Type::Const(Const::Count(n)))) } else { Never },
            Type::Bool   => self.to_str().parse().ok().then_always(|b| Type::Const(Const::Bool(b))),
            Type::Color  => self.to_str().parse().ok().then_always(|RichColor(x)| Type::Const(Const::Color(x))),
            Type::Coords => self.to_str().parse().ok().then_always(|Coords(x)| Type::Const(Const::Coords(x))),
            Type::Tempo  => self.to_str().parse().ok().then_always(|x| Type::Const(Const::Tempo(x))),
            Type::Ranged(t) => match t {
                Ranged::Text(opts) => {
                    let s = self.to_str();
                    opts.contains(&s)
                        .then_always(|| Type::Const(Const::Text(s)))
                },
                Ranged::Cmd(opts) => {
                    self.to_str()
                        .parse().ok()
                        .filter(|c| opts.contains(&c))
                        .then_always(|c| Type::Const(Const::Cmd(c)))
                },
                Ranged::Count(_ranges) => todo!(),
            },
            Type::Const(v) => match v {
                Const::Text(v) => matches!(self as &[Token], [tkn] if tkn.is_str() && tkn.src == *v),
                Const::Cmd(v) => self.to_str().parse().ok().is_some_and(|c: Cmd| c == *v),
                Const::Count(v) => if let [tkn] = self as &[Token] {
                    tkn.is_number() && tkn.src.parse().is_ok_and(|n: usize| n == *v)
                } else {
                    false
                },
                Const::Bool(v) => if let [tkn] = self as &[Token] {
                    matches!((v, tkn.src), (true, "true"|"1") | (false, "false"|"0"))
                } else {
                    false
                },
                Const::Color(v)  => self.to_str().parse().ok().is_some_and(|RichColor(x)| &x == v),
                Const::Coords(v) => self.to_str().parse().ok().is_some_and(|Coords(x)| &x == v),
                Const::Tempo(v)  => self.to_str().parse().ok().is_some_and(|x: Tempo| &x == v),
            }.then_always(|| *into)
        }
    }
}

impl CoerceArg for Type<'_> {
    fn coerce_arg<'a>(&'a self, into: &'a Type<'a>) -> Coercion<Type<'a>> where Self: 'a {
        use Coercion::*;

        match (into, self) {
            | (Type::Any, _)
            | (Type::Text, Self::Text | Self::Ranged(Ranged::Text(_)) | Self::Const(Const::Text(_)))
            | (Type::Cmd, Self::Cmd | Self::Ranged(Ranged::Cmd(_)) | Self::Const(Const::Cmd(_)))
            | (Type::Count, Self::Count | Self::Ranged(Ranged::Count(_)) | Self::Const(Const::Count(_)))
            | (Type::Bool, Self::Bool | Self::Const(Const::Bool(_)))
            | (Type::Color, Self::Color | Self::Const(Const::Color(_)))
            | (Type::Coords, Self::Coords | Self::Const(Const::Coords(_)))
            | (Type::Tempo, Self::Tempo | Self::Const(Const::Tempo(_)))
            | (Type::Ranged(Ranged::Text(_)), Self::Text)
            | (Type::Ranged(Ranged::Cmd(_)), Self::Cmd)
            | (Type::Ranged(Ranged::Count(_)), Self::Count)
                => Always(*self),

            // todo

            _ => Never,
        }
    }
}
