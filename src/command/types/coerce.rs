use crate::{command::{lex::{adjacent::AdjTokens, Token}, Cmd}, types::{Coords, RichColor, Tempo}};
use super::{Type, TypeBounds};

pub trait CoerceArg {
    /// Converts to the most specific subset of `into` that `self` belongs to.
    ///
    /// # Returns
    ///
    /// ## `Some((true, _))`
    /// The argument is a subset of or exactly `into` and is guaranteed to coerce.
    /// This should never cause a compile error nor warning, because it is correct.
    ///
    /// ### Examples
    /// - `T -> any` A type can always coerce to `any`.
    /// - `str -> str` A type can always coerce to itself.
    /// - `"squeak" -> str` A string constant can always coerce to a string.
    /// - `[T; 5] -> [T; 2..=7]` An array of len `n` can always coerce to an array of len `l..=h` where `l<=n<=h`.
    ///
    /// ## `Some((false, _))`
    /// The argument is of a superset of `into` and may belong to it, but is not guaranteed to.
    /// This should never cause a compile error, however it may warrent a warning and should be used in diagnostics.
    ///
    /// ### Examples
    /// - `any -> T` It is possible that `any` is `T`, but it might not be.
    /// - `str -> "squeak"` It is possible that a string might be "squeak", but it might be "orange" or "sqrk".
    /// - `[T; 2..] -> [T; 2]` It is possible that an array of length 2 or greater is exactly 2, but it could be any number of elements.
    ///
    /// ## `None`
    /// The argument has no overlap with `into` and is guaranteed to fail.
    ///
    /// ### Examples
    /// - `"apple" -> "orange"` Two string literals that are unequal will never coerce to one another.
    /// - `[T; 3] -> [T; 5]` An array will never coerce to an array of incompatible length.
    /// - `bool -> uint` Booleans are provided as "true" or "false", which would be confusing if implicitly coerced to integers.
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
                        .map(|&opt| (true, Type::ConstIdent(opt)))
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
    fn coerce_arg(&self, _into: &Type) -> Option<(bool, Type)> {
        todo!()
    }
}

impl CoerceArg for TypeBounds {
    fn coerce_arg(&self, _into: &Type) -> Option<(bool, Type)> {
        todo!()
    }
}
