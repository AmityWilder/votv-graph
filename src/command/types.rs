use crate::graph::VertexID;
pub use crate::{command::{Cmd, ProgramData}, types::{Coords, RichColor, Tempo}};
use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct SizeHint {
    pub low: usize,
    pub high: Option<usize>,
}

impl From<std::ops::RangeInclusive<usize>> for SizeHint {
    fn from(value: std::ops::RangeInclusive<usize>) -> Self {
        Self {
            low: *value.start(),
            high: Some(*value.end()),
        }
    }
}
impl From<std::ops::RangeFrom<usize>> for SizeHint {
    fn from(value: std::ops::RangeFrom<usize>) -> Self {
        Self {
            low: value.start,
            high: None,
        }
    }
}
impl From<std::ops::RangeToInclusive<usize>> for SizeHint {
    fn from(value: std::ops::RangeToInclusive<usize>) -> Self {
        Self {
            low: 0,
            high: Some(value.end),
        }
    }
}
impl From<std::ops::RangeFull> for SizeHint {
    fn from(_value: std::ops::RangeFull) -> Self {
        Self {
            low: 0,
            high: None,
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

#[derive(Debug, Clone, Default)]
pub enum Type {
    #[default]
    Void,
    Text,
    Cmd,
    Vertex,
    Bool,
    Index,
    Scalar,
    Coords,
    Color,
    Tempo,
    Tuple(&'static [Type]),
    Union(&'static [Type]),
    Array(&'static Type, SizeHint),
}

#[derive(Debug, Clone, Default)]
pub enum Value {
    #[default]
    Void,
    Text(String),
    Cmd(Cmd),
    Vertex(VertexID),
    Bool(bool),
    Index(usize),
    Scalar(f32),
    Coords(Coords),
    Color(RichColor),
    Tempo(Tempo),
    Multi(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => Ok(()),

            Self::Text  (inner) => inner.fmt(f),
            Self::Cmd   (inner) => inner.fmt(f),
            Self::Vertex(inner) => inner.fmt(f),
            Self::Bool  (inner) => inner.fmt(f),
            Self::Index (inner) => inner.fmt(f),
            Self::Scalar(inner) => inner.fmt(f),
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

impl Value {
    /// Same type without coercion
    pub fn is_type(&self, t: &Type) -> bool {
        match t {
            // T == T
            Type::Void   => matches!(self, Value::Void),
            Type::Text   => matches!(self, Value::Text  (_)),
            Type::Cmd    => matches!(self, Value::Cmd   (_)),
            Type::Vertex => matches!(self, Value::Vertex(_)),
            Type::Bool   => matches!(self, Value::Bool  (_)),
            Type::Index  => matches!(self, Value::Index (_)),
            Type::Scalar => matches!(self, Value::Scalar(_)),
            Type::Coords => matches!(self, Value::Coords(_)),
            Type::Color  => matches!(self, Value::Color (_)),
            Type::Tempo  => matches!(self, Value::Tempo (_)),

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
        }
    }

    pub fn coerce_parse(s: &str, t: &Type, data: &ProgramData) -> Option<Value> {
        let result = match t {
            // "" -> ()
            Type::Void => s.is_empty().then_some(Value::Void),

            // str -> str
            // (should normally be handled by `coerce_to`, but just in case...)
            Type::Text => Some(Value::Text(s.to_string())),

            // str -> T
            Type::Cmd    => s.parse().ok().map(Value::Cmd   ),
            Type::Bool   => s.parse().ok().map(Value::Bool  ),
            Type::Index  => s.parse().ok().map(Value::Index ),
            Type::Scalar => s.parse().ok().map(Value::Scalar),
            Type::Coords => s.parse().ok().map(Value::Coords),
            Type::Color  => s.parse().ok().map(Value::Color ),
            Type::Tempo  => s.parse().ok().map(Value::Tempo ),

            // "<ID or alias>" -> VertexID
            Type::Vertex => data.graph.find_vert(&s).map(Value::Vertex),

            // "<T str> <U str> <V str>" -> (T, U, V)
            Type::Tuple(ts) => {
                let mut s_it = s.split_whitespace();
                let mut t_it = ts.iter();
                let mut items = Vec::with_capacity(ts.len());
                loop {
                    let (s, t) = (s_it.next(), t_it.next());
                    if let (Some(s), Some(t)) = (s, t) {
                        items.push(Self::coerce_parse(s, t, data)?);
                    } else {
                        break (t.is_none() && s.is_none()).then_some(Value::Multi(items));
                    }
                }
            }

            Type::Union(ts) => {
                if ts.iter().any(|t| matches!(t, Type::Text)) {
                    // str -> T|str|U
                    Some(Value::Text(s.to_string()))
                } else {
                    // coerse to *which* variant?
                    None
                }
            }

            Type::Array(t, size) => {
                let mut s_it = s.split_whitespace();
                let n = s_it.clone().count();
                if size.contains(n) {
                    let mut items = Vec::with_capacity(n);
                    while let Some(s) = s_it.next() {
                        items.push(Self::coerce_parse(s, t, data)?);
                    }
                    // "<T str> <T str> <T str> ...{n}" where l<=n<=h -> [T; l..=h]
                    Some(Value::Multi(items))
                } else {
                    None
                }
            }
        };

        debug_assert!(result.as_ref().is_none_or(|value| value.is_type(t)), "type coercion should produce the 'into' type");

        result
    }

    pub fn coerce_to(mut self, t: &Type, data: &ProgramData) -> Option<Value> {
        let new_value = if self.is_type(t) {
            // T -> T
            Some(self)
        } else if let Value::Multi(items) = &mut self && matches!(t, Type::Void) && items.is_empty() {
            // [T; 0] -> ()
            Some(Value::Void)
        } else if let Type::Array(_, size) = t && size.contains(0) && matches!(self, Value::Void) {
            // () -> [T; 0..]
            Some(Value::Multi(Vec::new()))
        } else if let Value::Multi(items) = &mut self && let [item] = &items[..] && item.is_type(t) {
            // [T; 1] -> T
            Some(items.pop().expect("should be guarded by `[item] = &items[..]`"))
        } else if let Type::Array(t, size) = t && size.contains(1) && self.is_type(t) {
            // T -> [T; l..=h] where l<=1<=h
            Some(Value::Multi(vec![self]))
        } else if matches!(t, Type::Text) {
            // T -> str
            Some(Value::Text(self.to_string()))
        } else if let Value::Text(s) = self {
            // str -> T
            Self::coerce_parse(&s, t, data)
        } else {
            None
        };

        debug_assert!(new_value.as_ref().is_none_or(|value| value.is_type(t)), "type coercion should produce the 'into' type");

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

            (a, b) => Value::Multi(vec![a, b]),
        }
    }
}
