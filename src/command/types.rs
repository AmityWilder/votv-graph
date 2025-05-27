// use raylib::prelude::*;
use crate::{types::{ParseColorError, ParseCoordsError, ParseTempoError}};
pub use crate::{command::Cmd, types::{Coords, RichColor, Tempo}};
use std::str::FromStr;

use super::CmdError;

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

#[derive(Debug)]
pub enum BoundsError {
    FailCondition(String),
}
impl std::fmt::Display for BoundsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BoundsError::FailCondition(reason) => write!(f, "bounds failed: {reason}"),
        }
    }
}
impl std::error::Error for BoundsError {}

#[derive(Debug, Clone)]
pub enum Bounds {
    Text   { cond: fn(&str) -> String, pred: fn(&str      ) -> Result<(), BoundsError> },
    Cmd    { cond: fn(&str) -> String, pred: fn(&Cmd      ) -> Result<(), BoundsError> },
    Vertex { cond: fn(&str) -> String, pred: fn(&str      ) -> Result<(), BoundsError> },
    Bool   { cond: fn(&str) -> String, pred: fn(&bool     ) -> Result<(), BoundsError> },
    Count  { cond: fn(&str) -> String, pred: fn(&usize    ) -> Result<(), BoundsError> },
    Amount { cond: fn(&str) -> String, pred: fn(&f32      ) -> Result<(), BoundsError> },
    Coords { cond: fn(&str) -> String, pred: fn(&Coords   ) -> Result<(), BoundsError> },
    Color  { cond: fn(&str) -> String, pred: fn(&RichColor) -> Result<(), BoundsError> },
    Tempo  { cond: fn(&str) -> String, pred: fn(&Tempo    ) -> Result<(), BoundsError> },
    Tuple  { cond: fn(&str) -> String, pred: fn(&[Value]  ) -> Result<(), BoundsError>, ts: Vec<Type> },
    Union  { cond: fn(&str) -> String, pred: fn(&Value    ) -> Result<(), BoundsError>, ts: Vec<Type> },
    Array  { cond: fn(&str) -> String, pred: fn(&[Value]  ) -> Result<(), BoundsError>, t: Box<Type>, size: SizeHint },
}

impl PartialEq for Bounds {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Text   { pred: l_pred,                       .. }, Self::Text   { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Cmd    { pred: l_pred,                       .. }, Self::Cmd    { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Vertex { pred: l_pred,                       .. }, Self::Vertex { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Bool   { pred: l_pred,                       .. }, Self::Bool   { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Count  { pred: l_pred,                       .. }, Self::Count  { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Amount { pred: l_pred,                       .. }, Self::Amount { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Coords { pred: l_pred,                       .. }, Self::Coords { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Color  { pred: l_pred,                       .. }, Self::Color  { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Tempo  { pred: l_pred,                       .. }, Self::Tempo  { pred: r_pred,                       .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred),
            (Self::Tuple  { pred: l_pred, ts: l_ts,             .. }, Self::Tuple  { pred: r_pred, ts: r_ts,             .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred) && l_ts == r_ts,
            (Self::Union  { pred: l_pred, ts: l_ts,             .. }, Self::Union  { pred: r_pred, ts: r_ts,             .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred) && l_ts == r_ts,
            (Self::Array  { pred: l_pred, t: l_t, size: l_size, .. }, Self::Array  { pred: r_pred, t: r_t, size: r_size, .. }) => std::ptr::fn_addr_eq(*l_pred, *r_pred) && l_size == r_size && l_t == r_t,
            _ => false,
        }
    }
}

impl Bounds {
    pub fn inner_type(&self) -> Type {
        match self {
            Self::Text  {..} => Type::Text,
            Self::Cmd   {..} => Type::Cmd,
            Self::Vertex{..} => Type::Vertex,
            Self::Bool  {..} => Type::Bool,
            Self::Count {..} => Type::Count,
            Self::Amount{..} => Type::Amount,
            Self::Coords{..} => Type::Coords,
            Self::Color {..} => Type::Color,
            Self::Tempo {..} => Type::Tempo,
            Self::Tuple { ts, .. } => Type::Tuple(ts.clone()),
            Self::Union { ts, .. } => Type::Union(ts.clone()),
            Self::Array { t, size, .. } => Type::Array(t.clone(), *size),
        }
    }
}

impl std::fmt::Display for Bounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let local = "x";
        let (inner, cond) = (self.inner_type(), match self {
            | Bounds::Text   { cond, .. }
            | Bounds::Cmd    { cond, .. }
            | Bounds::Vertex { cond, .. }
            | Bounds::Bool   { cond, .. }
            | Bounds::Count  { cond, .. }
            | Bounds::Amount { cond, .. }
            | Bounds::Coords { cond, .. }
            | Bounds::Color  { cond, .. }
            | Bounds::Tempo  { cond, .. }
            | Bounds::Tuple  { cond, .. }
            | Bounds::Union  { cond, .. }
            | Bounds::Array  { cond, .. }
                => cond(local),
        });

        write!(f, "{inner} {local} where {cond}")
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Type {
    #[default]
    Void,
    Text,
    Literal(&'static str),
    Cmd,
    Vertex,
    Bool,
    Count,
    Amount,
    Coords,
    Color,
    Tempo,
    Tuple(Vec<Type>),
    Union(Vec<Type>),
    Array(Box<Type>, SizeHint),
    Bounded(Bounds),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void         => "()".fmt(f),
            Type::Text         => "str".fmt(f),
            Type::Literal(lit) => lit.fmt(f),
            Type::Cmd          => "cmd".fmt(f),
            Type::Vertex       => "vertex".fmt(f),
            Type::Bool         => "bool".fmt(f),
            Type::Count        => "uint".fmt(f),
            Type::Amount       => "float".fmt(f),
            Type::Coords       => "coords".fmt(f),
            Type::Color        => "color".fmt(f),
            Type::Tempo        => "tempo".fmt(f),
            Type::Tuple(ts) => {
                let mut it = ts.iter();
                '('.fmt(f)?;
                if let Some(t) = it.next() {
                    t.fmt(f)?;
                    for t in it {
                        write!(f, ", {t}")?;
                    }
                }
                ')'.fmt(f)
            }
            Type::Union(ts) => {
                let mut it = ts.iter();
                if let Some(t) = it.next() {
                    t.fmt(f)?;
                    for t in it {
                        write!(f, "|{t}")?;
                    }
                }
                Ok(())
            }
            Type::Array(t, size_hint) => write!(f, "[{t}; {size_hint}]"),
            Type::Bounded(bounds) => bounds.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum Value {
    #[default]
    Void,
    Text(String),
    Lit(&'static str),
    Cmd(Cmd),
    Vertex(String),
    Bool(bool),
    Count(usize),
    Amount(f32),
    Coords(Coords),
    Color(RichColor),
    Tempo(Tempo),
    Multi(Vec<Value>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => Ok(()),

            Self::Text  (inner) => inner.fmt(f),
            Self::Lit   (inner) => inner.fmt(f),
            Self::Cmd   (inner) => inner.fmt(f),
            Self::Vertex(inner) => inner.fmt(f),
            Self::Bool  (inner) => inner.fmt(f),
            Self::Count (inner) => inner.fmt(f),
            Self::Amount(inner) => inner.fmt(f),
            Self::Coords(inner) => inner.fmt(f),
            Self::Color (inner) => inner.fmt(f),
            Self::Tempo (inner) => inner.fmt(f),

            Self::Multi (inner) => {
                if let [first, rest @ ..] = &inner[..] {
                    first.fmt(f)?;
                    for item in rest {
                        use std::fmt::Write;
                        f.write_char(' ')?;
                        item.fmt(f)?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl FromStr for Value {
    type Err = !;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut items = s.split_whitespace()
            .map(|s| {
                if s.starts_with(|c: char| c.is_ascii_digit()) {
                    if let Ok(x) = s.parse() {
                        return Value::Count(x);
                    }
                } else if s.strip_prefix('-').unwrap_or(s).starts_with(|c: char| c.is_ascii_digit() || c == '.') {
                    if let Ok(x) = s.parse() {
                        return Value::Amount(x);
                    }
                } if let Ok(x) = s.parse::<bool>() {
                    return Value::Bool(x);
                } else if s.starts_with('#') || s.starts_with("rgb(") || s.starts_with("rgba(") {
                    if let Ok(x) = s.parse() {
                        return Value::Color(x);
                    }
                } else if s.starts_with("x:") {
                    if let Ok(x) = s.parse() {
                        return Value::Coords(x);
                    }
                } else if s.starts_with("ticks:") {
                    if let Ok(x) = s.parse() {
                        return Value::Tempo(x);
                    }
                } else if let Ok(x) = s.parse() {
                    return Value::Cmd(x);
                } else if let Ok(x) = s.parse() {
                    return Value::Color(x);
                }
                Value::Text(s.to_string())
            })
            .collect::<Vec<Value>>();

        Ok(match items.len() {
            0 => Value::Void,
            1 => items.pop().unwrap(),
            _ => Value::Multi(items),
        })
    }
}

#[derive(Debug)]
pub enum CoerceError {
    Incompatible { expect: Type, actual: Type },
    Ambiguous { possible: Vec<Type> },
    Bounds(BoundsError),
    ParseInt(std::num::ParseIntError),
    ParseFloat(std::num::ParseFloatError),
    ParseBool(std::str::ParseBoolError),
    ParseCmd(CmdError),
    ParseColor(ParseColorError),
    ParseCoords(ParseCoordsError),
    ParseTempo(ParseTempoError),
}
impl std::fmt::Display for CoerceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Incompatible { expect, actual } => write!(f, "type mismatch: `{actual}` is not compatible with `{expect}`"),
            Self::Ambiguous { possible } => {
                "ambiguous coercion target; could be any of ".fmt(f)?;
                let mut it = possible.into_iter();
                if let Some(t) = it.next() {
                    t.fmt(f)?;
                    for t in possible {
                        write!(f, ", {t}")?;
                    }
                }
                Ok(())
            }
            Self::Bounds(_) => "coercion passed, but bounds were not met".fmt(f),

            | Self::ParseInt(_)
            | Self::ParseFloat(_)
            | Self::ParseBool(_)
            | Self::ParseCmd(_)
            | Self::ParseColor(_)
            | Self::ParseCoords(_)
            | Self::ParseTempo(_)
                => "parse error".fmt(f),
        }
    }
}
impl std::error::Error for CoerceError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            | Self::Incompatible { .. }
            | Self::Ambiguous { .. }
                => None,

            Self::Bounds(e) => Some(e),
            Self::ParseInt(e) => Some(e),
            Self::ParseFloat(e) => Some(e),
            Self::ParseBool(e) => Some(e),
            Self::ParseCmd(e) => Some(e),
            Self::ParseColor(e) => Some(e),
            Self::ParseCoords(e) => Some(e),
            Self::ParseTempo(e) => Some(e),
        }
    }
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Void      => Type::Void,
            Value::Text  (_) => Type::Text,
            Value::Lit   (s) => Type::Literal(*s),
            Value::Cmd   (_) => Type::Cmd,
            Value::Vertex(_) => Type::Vertex,
            Value::Bool  (_) => Type::Bool,
            Value::Count (_) => Type::Count,
            Value::Amount(_) => Type::Amount,
            Value::Coords(_) => Type::Coords,
            Value::Color (_) => Type::Color,
            Value::Tempo (_) => Type::Tempo,
            Value::Multi (v) => {
                let ts = v.iter().map(|x| x.get_type()).collect::<Vec<Type>>();
                let mut it = ts.iter();
                if let Some(t0) = it.next() {
                    if it.all(|t| t == t0) {
                        Type::Array(Box::new(t0.clone()), SizeHint::new(ts.len()))
                    } else {
                        Type::Tuple(ts)
                    }
                } else {
                    Type::Array(Box::new(Type::Text), SizeHint::new(0))
                }
            }
        }
    }

    fn bounds_test(&self, bound: &Bounds) -> Result<(), CoerceError> {
        let inner_type = bound.inner_type();
        let passes_vibe_check = self.is_type(&inner_type);
        match (passes_vibe_check, bound, self) {
            (true, Bounds::Text   { pred, .. }, Value::Text  (value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Cmd    { pred, .. }, Value::Cmd   (value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Vertex { pred, .. }, Value::Vertex(value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Bool   { pred, .. }, Value::Bool  (value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Count  { pred, .. }, Value::Count (value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Amount { pred, .. }, Value::Amount(value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Coords { pred, .. }, Value::Coords(value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Color  { pred, .. }, Value::Color (value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Tempo  { pred, .. }, Value::Tempo (value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Tuple  { pred, .. }, Value::Multi (value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Union  { pred, .. },               value ) => pred(value).map_err(|e| CoerceError::Bounds(e)),
            (true, Bounds::Array  { pred, .. }, Value::Multi (value)) => pred(value).map_err(|e| CoerceError::Bounds(e)),

            _ => Err(CoerceError::Incompatible { expect: inner_type, actual: self.get_type() }),
        }
    }

    /// Same type without coercion
    pub fn is_type(&self, t: &Type) -> bool {
        match t {
            // T == T
            Type::Void   => matches!(self, Value::Void),
            Type::Text   => matches!(self, Value::Text  (_)),
            Type::Cmd    => matches!(self, Value::Cmd   (_)),
            Type::Vertex => matches!(self, Value::Vertex(_)),
            Type::Bool   => matches!(self, Value::Bool  (_)),
            Type::Count  => matches!(self, Value::Count (_)),
            Type::Amount => matches!(self, Value::Amount(_)),
            Type::Coords => matches!(self, Value::Coords(_)),
            Type::Color  => matches!(self, Value::Color (_)),
            Type::Tempo  => matches!(self, Value::Tempo (_)),

            Type::Literal(lit) => matches!(self, Value::Text(s) if s == lit),

            // T == T|U|V
            Type::Union(ts) => ts.iter().any(|t| self.is_type(t)),

            // [T; n] == [T; l..=h] where l<=n<=h
            Type::Array(t, size) => {
                if let Value::Multi(items) = self {
                    size.contains(items.len()) &&
                        items.iter().all(|item| item.is_type(t))
                } else { false }
            }

            // (T, U, V) == (T, U, V)
            Type::Tuple(ts) => {
                if let Value::Multi(items) = self {
                    ts.len() == items.len() &&
                        ts.iter().zip(items.iter()).all(|(t, item)| item.is_type(t))
                } else { false }
            }

            Type::Bounded(bounds) => self.bounds_test(bounds).is_ok(),
        }
    }

    fn coerce_parse(s: &str, t: &Type) -> Result<Value, CoerceError> {
        let result = match t {
            // "" -> ()
            Type::Void => s.is_empty().then_some(Value::Void)
                .ok_or_else(|| CoerceError::Incompatible { expect: t.clone(), actual: Type::Text }),

            // str -> str
            // (should normally be handled by `coerce_to`, but just in case...)
            Type::Text => Ok(Value::Text(s.to_string())),
            Type::Literal(lit) => (s == *lit).then_some(Value::Lit(lit))
                .ok_or_else(|| CoerceError::Incompatible { expect: t.clone(), actual: Type::Text }),

            // str -> T
            Type::Bool   => s.parse().map_err(|e| CoerceError::ParseBool  (e)).map(Value::Bool  ),
            Type::Count  => s.parse().map_err(|e| CoerceError::ParseInt   (e)).map(Value::Count ),
            Type::Amount => s.parse().map_err(|e| CoerceError::ParseFloat (e)).map(Value::Amount),
            Type::Cmd    => s.parse().map_err(|e| CoerceError::ParseCmd   (e)).map(Value::Cmd   ),
            Type::Color  => s.parse().map_err(|e| CoerceError::ParseColor (e)).map(Value::Color ),
            Type::Tempo  => s.parse().map_err(|e| CoerceError::ParseTempo (e)).map(Value::Tempo ),
            Type::Coords => s.parse().map_err(|e| CoerceError::ParseCoords(e)).map(Value::Coords),

            // "<ID or alias>" -> VertexID
            Type::Vertex => Ok(Value::Vertex(s.to_string())),

            // "<T str> <U str> <V str>" -> (T, U, V)
            Type::Tuple(ts) => {
                let mut from_it = s.split_whitespace();
                let mut into_it = ts.iter();
                let mut items = Vec::with_capacity(ts.len());
                loop {
                    let (from, into) = (from_it.next(), into_it.next());
                    if let (Some(from), Some(into)) = (from, into) {
                        items.push(Self::coerce_parse(from, into)?);
                    } else {
                        break (into.is_none() && from.is_none()).then_some(Value::Multi(items))
                            .ok_or_else(|| CoerceError::Incompatible { expect: t.clone(), actual: s.parse::<Value>().unwrap().get_type() });
                    }
                }
            }

            Type::Union(ts) => {
                if ts.iter().any(|t| matches!(t, Type::Text)) {
                    // str -> T|str|U
                    Ok(Value::Text(s.to_string()))
                } else {
                    // coerse to *which* variant?
                    Err(CoerceError::Ambiguous { possible: ts.to_vec() })
                }
            }

            Type::Array(shared_t, size) => {
                let mut s_it = s.split_whitespace();
                let n = s_it.clone().count();
                if size.contains(n) {
                    let mut items = Vec::with_capacity(n);
                    while let Some(s) = s_it.next() {
                        items.push(Self::coerce_parse(s, shared_t)?);
                    }
                    // "<T str> <T str> <T str> ...{n}" where l<=n<=h -> [T; l..=h]
                    Ok(Value::Multi(items))
                } else {
                    Err(CoerceError::Incompatible { expect: t.clone(), actual: s.parse::<Value>().unwrap().get_type() })
                }
            }

            Type::Bounded(bound) => Self::coerce_parse(s, &bound.inner_type())
                .and_then(|value| value.bounds_test(bound).map(|()| value)),
        };

        debug_assert!(result.is_err() || result.as_ref().is_ok_and(|value| value.is_type(t)), "type coercion should produce the 'into' type");

        result
    }

    pub fn coerce_to(mut self, t: &Type) -> Result<Value, CoerceError> {
        let mut loop_count = 0;
        let new_value = loop {
            let result = if self.is_type(t) {
                // T -> T
                Ok(self)
            } else if let Value::Multi(items) = &mut self && matches!(t, Type::Void) && items.is_empty() {
                // [T; 0] -> ()
                Ok(Value::Void)
            } else if let Type::Array(_, size) = t && size.contains(0) && matches!(self, Value::Void) {
                // () -> [T; 0..]
                Ok(Value::Multi(Vec::new()))
            } else if let Value::Multi(items) = &mut self && let [item] = &items[..] && item.is_type(t) {
                // [T; 1] -> T
                Ok(items.pop().expect("should be guarded by `[item] = &items[..]`"))
            } else if let Type::Array(t, size) = t && size.contains(1) && self.is_type(t) {
                // T -> [T; l..=h] where l<=1<=h
                Ok(Value::Multi(vec![self]))
            } else if matches!(t, Type::Text) {
                // T -> str
                Ok(Value::Text(self.to_string()))
            } else if let Value::Text(s) = self {
                // str -> T
                Self::coerce_parse(&s, t)
            } else if let Type::Bounded(bound) = t {
                self.coerce_to(&bound.inner_type()).and_then(|value| value.bounds_test(bound).map(|()| value))
            } else {
                Err(CoerceError::Incompatible { expect: t.clone(), actual: self.get_type() })
            };

            if result.as_ref().is_ok_and(|value| !value.is_type(t)) {
                self = result.unwrap();
                assert!(loop_count < 32, "coercion overflow, possible loop");
                loop_count += 1;
                continue;
            } else {
                break result;
            }
        };

        debug_assert!(new_value.is_err() || new_value.as_ref().is_ok_and(|value| value.is_type(t)), "type coercion should produce the 'into' type");

        new_value
    }

    pub fn coerce_append(self, value: Value) -> Value {
        match (self, value) {
            // () + T -> T
            // T + () -> T
            (Value::Void, x) | (x, Value::Void) => x,

            // [T; n] + [T; m] -> [T; n+m]
            // [T; n] + [U; m] -> [T|U; n+m]
            // [T; n] + [U; m] -> (T, T, ...{n}, U, U, ...{m})
            (Value::Multi(mut v1), Value::Multi(mut v2)) => {
                v1.append(&mut v2);
                Value::Multi(v1)
            }

            // [T; n] + T -> [T; n+1]
            // [T; n] + U -> [T|U; n+1]
            // [T; n] + U -> (T, T, ...{n}, U)
            (Value::Multi(mut v), x) => {
                v.push(x);
                Value::Multi(v)
            }

            // T + [T; n] -> [T; n+1]
            // T + [U; n] -> [T|U; n+1]
            // T + [U; n] -> (T, U, U, ...{n})
            (x, Value::Multi(mut v)) => {
                v.insert(0, x);
                Value::Multi(v)
            }

            // T + T -> [T; 2]
            // T + U -> [T|U; 2]
            // T + U -> (T, U)
            (a, b) => Value::Multi(vec![a, b]),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coercion() {
        let value = "squeak #e73bc8 red x:20/y:45 apple orange".parse::<Value>().expect("parse failed");
        assert!(value.is_type(&Type::Tuple(vec![Type::Text, Type::Color, Type::Color, Type::Coords, Type::Text, Type::Color])));
        let new_type: Type = Type::Tuple(vec![Type::Literal("squeak"), Type::Color, Type::Color, Type::Coords, Type::Literal("apple"), Type::Color]);
        let new_value = value.coerce_to(&new_type);
        assert!(new_value.as_ref().is_ok_and(|x| x.is_type(&new_type)), "{:?}", new_value.unwrap_err());
    }

    #[test]
    fn test_bounds() {
        let value = "appleeeee".parse::<Value>().expect("parse failed");
        assert!(value.is_type(&Type::Text));
        let new_type: Type = Type::Bounded(Bounds::Text {
            cond: |x| format!("{x} does not end with 'e'"),
            pred: |s| if !s.ends_with('e') { Err(BoundsError::FailCondition(format!("{s:?} ends with 'e'"))) } else { Ok(()) },
        });
        let new_value = value.coerce_to(&new_type);
        assert!(new_value.as_ref().is_ok_and(|x| x.is_type(&new_type)), "{:?}", new_value.unwrap_err());
    }
}
