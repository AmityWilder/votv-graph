use std::fmt::Display;

use super::{concept::{ArrayType, CompoundType, SimpleType, TupleType, Type}, *};

pub enum Simple {
    Text(Text),
    Cmd(Cmd),
    Vertex(Vertex),
    Bool(Bool),
    Index(Index),
    Scalar(Scalar),
    Coords(Coords),
    Color(Color),
    Tempo(Tempo),
}

impl Display for Simple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Simple::Text  (inner) => inner.fmt(f),
            Simple::Cmd   (inner) => inner.fmt(f),
            Simple::Vertex(inner) => inner.fmt(f),
            Simple::Bool  (inner) => inner.fmt(f),
            Simple::Index (inner) => inner.fmt(f),
            Simple::Scalar(inner) => inner.fmt(f),
            Simple::Coords(inner) => inner.fmt(f),
            Simple::Color (inner) => inner.fmt(f),
            Simple::Tempo (inner) => inner.fmt(f),
        }
    }
}

impl Simple {
    pub fn storage_type(&self) -> SimpleType {
        match self {
            Self::Text  (_) => SimpleType::Text,
            Self::Cmd   (_) => SimpleType::Cmd,
            Self::Vertex(_) => SimpleType::Vertex,
            Self::Bool  (_) => SimpleType::Bool,
            Self::Index (_) => SimpleType::Index,
            Self::Scalar(_) => SimpleType::Scalar,
            Self::Coords(_) => SimpleType::Coords,
            Self::Color (_) => SimpleType::Color,
            Self::Tempo (_) => SimpleType::Tempo,
        }
    }
}

pub struct Tuple {
    items: Vec<Value>,
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.items.iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ")
            .fmt(f)
    }
}

impl Tuple {
    pub fn storage_type(&self) -> TupleType {
        let ts = self.items.iter()
            .map(|item| item.storage_type())
            .collect();

        TupleType { ts }
    }
}

pub enum SimpleArray {
    Text  (Vec<Text  >),
    Cmd   (Vec<Cmd   >),
    Vertex(Vec<Vertex>),
    Bool  (Vec<Bool  >),
    Index (Vec<Index >),
    Scalar(Vec<Scalar>),
    Coords(Vec<Coords>),
    Color (Vec<Color >),
    Tempo (Vec<Tempo >),
}

impl Display for SimpleArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text  (inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Cmd   (inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Vertex(inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Bool  (inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Index (inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Scalar(inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Coords(inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Color (inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Tempo (inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
        }.join(" ").fmt(f)
    }
}

impl SimpleArray {
    pub fn storage_type(&self) -> (SimpleType, usize) {
        match self {
            Self::Text  (v) => (SimpleType::Text,   v.len()),
            Self::Cmd   (v) => (SimpleType::Cmd,    v.len()),
            Self::Vertex(v) => (SimpleType::Vertex, v.len()),
            Self::Bool  (v) => (SimpleType::Bool,   v.len()),
            Self::Index (v) => (SimpleType::Index,  v.len()),
            Self::Scalar(v) => (SimpleType::Scalar, v.len()),
            Self::Coords(v) => (SimpleType::Coords, v.len()),
            Self::Color (v) => (SimpleType::Color,  v.len()),
            Self::Tempo (v) => (SimpleType::Tempo,  v.len()),
        }
    }
}

pub enum CompoundArray {
    Tuple (TupleType, Vec<Tuple>),
    Array (ArrayType, Vec<Array>),
}

impl Display for CompoundArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tuple(_, inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
            Self::Array(_, inner) => inner.iter().map(ToString::to_string).collect::<Vec<_>>(),
        }.join(" ").fmt(f)
    }
}

impl CompoundArray {
    pub fn storage_type(&self) -> (CompoundType, usize) {
        match self {
            Self::Tuple(t, v) => (CompoundType::Tuple(t.clone()), v.len()),
            Self::Array(t, v) => (CompoundType::Array(t.clone()), v.len()),
        }
    }
}

pub enum Array {
    Simple(SimpleArray),
    Compound(CompoundArray),
}

impl Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple(simple) => simple.fmt(f),
            Self::Compound(compound) => compound.fmt(f),
        }
    }
}

impl Array {
    pub fn storage_type(&self) -> ArrayType {
        let (t, len) = match self {
            Self::Simple(simple) => {
                let (t, len) = simple.storage_type();
                (Type::Simple(t), len)
            }
            Self::Compound(compound) => {
                let (t, len) = compound.storage_type();
                (Type::Compound(t), len)
            }
        };

        ArrayType {
            t: Box::new(t),
            size: (len, Some(len)),
        }
    }
}

pub enum Compound {
    Tuple(Tuple),
    Array(Array),
}

impl Display for Compound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tuple(inner) => inner.fmt(f),
            Self::Array(inner) => inner.fmt(f),
        }
    }
}

impl Compound {
    pub fn storage_type(&self) -> CompoundType {
        match self {
            Self::Tuple(tuple) => CompoundType::Tuple(tuple.storage_type()),
            Self::Array(array) => CompoundType::Array(array.storage_type()),
        }
    }
}

pub enum Value {
    Void,
    Simple(Simple),
    Compound(Compound),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => Ok(()),
            Self::Simple(inner) => inner.fmt(f),
            Self::Compound(inner) => inner.fmt(f),
        }
    }
}

impl Value {
    pub fn storage_type(&self) -> Type {
        match self {
            Self::Void => Type::Void,
            Self::Simple(simple) => Type::Simple(simple.storage_type()),
            Self::Compound(compound) => Type::Compound(compound.storage_type()),
        }
    }

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
