use std::fmt::Display;
use super::{concept::*, Cmd, Coords, RichColor, Tempo};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
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
    Tuple(Vec<Type>),
    Enum(Vec<Type>),
    Array(Box<Type>, (usize, Option<usize>)),
}

pub enum Value {
    Text(String),
    Cmd(Cmd),
    Vertex(String),
    Bool(bool),
    Index(usize),
    Scalar(f32),
    Coords(Coords),
    Color(RichColor),
    Tempo(Tempo),
    Array(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text  (inner) => inner.fmt(f),
            Self::Cmd   (inner) => inner.fmt(f),
            Self::Vertex(inner) => inner.fmt(f),
            Self::Bool  (inner) => inner.fmt(f),
            Self::Index (inner) => inner.fmt(f),
            Self::Scalar(inner) => inner.fmt(f),
            Self::Coords(inner) => inner.fmt(f),
            Self::Color (inner) => inner.fmt(f),
            Self::Tempo (inner) => inner.fmt(f),
            Self::Array (inner) => {
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
    pub fn coerce_to(self, ty: Type) -> Option<Value> {
        if let Type::Simple(ty) = ty {
            if ty == SimpleType::Text {
                return Some(Value::Simple(Simple::Text(match self {
                    Value::Void => String::new(),
                    Value::Simple(Simple::Text(s)) => s,
                    _ => self.to_string(),
                })));
            } else if let Value::Simple(Simple::Text(s)) = self {
                return match ty {
                    SimpleType::Text   => Some(SimpleType::Text(s)),
                    SimpleType::Cmd    => s.parse().ok().map(Simple::Cmd   ),
                    SimpleType::Vertex => s.parse().ok().map(Simple::Vertex),
                    SimpleType::Bool   => s.parse().ok().map(Simple::Bool  ),
                    SimpleType::Index  => s.parse().ok().map(Simple::Index ),
                    SimpleType::Scalar => s.parse().ok().map(Simple::Scalar),
                    SimpleType::Coords => s.parse().ok().map(Simple::Coords),
                    SimpleType::Color  => s.parse().ok().map(Simple::Color ),
                    SimpleType::Tempo  => s.parse().ok().map(Simple::Tempo ),
                }.map(Value::Simple);
            }
        }
    }
}
