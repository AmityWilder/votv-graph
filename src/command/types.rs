use crate::graph::VertexID;
pub use crate::{command::Cmd, types::{Coords, RichColor, Tempo}};
use std::fmt::Display;

use super::ProgramData;

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
    Array(Vec<Value>),
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

fn is_between(n: usize, min: usize, max: Option<usize>) -> bool {
    min <= n && max.is_none_or(|max| n <= max)
}

impl Value {
    pub fn coerce_to(self, ty: Type, data: &ProgramData) -> Option<Value> {
        match (&self, &ty) {
            // T -> T
            | (Value::Void,      Type::Void  )
            | (Value::Text  (_), Type::Text  )
            | (Value::Cmd   (_), Type::Cmd   )
            | (Value::Vertex(_), Type::Vertex)
            | (Value::Bool  (_), Type::Bool  )
            | (Value::Index (_), Type::Index )
            | (Value::Scalar(_), Type::Scalar)
            | (Value::Coords(_), Type::Coords)
            | (Value::Color (_), Type::Color )
            | (Value::Tempo (_), Type::Tempo )
                => Some(self),

            // [T; 0] -> [U; 0..]
            (Value::Array(v), Type::Array(_, (0, _))) if v.is_empty() => Some(self),

            // [T; 0] -> ()
            (Value::Array(v), Type::Void) if v.is_empty() => Some(self),

            // [T; l<=n<=h] -> [T; l..=h]
            (Value::Array(v), &Type::Array(t, (l, h))) if is_between(v.len(), l, h) && v => Some(self),

            // T -> [T; 1..]
            | (Value::Void,      Type::Array(box Type::Void,   (1, _)))
            | (Value::Text  (_), Type::Array(box Type::Text,   (1, _)))
            | (Value::Cmd   (_), Type::Array(box Type::Cmd,    (1, _)))
            | (Value::Vertex(_), Type::Array(box Type::Vertex, (1, _)))
            | (Value::Bool  (_), Type::Array(box Type::Bool,   (1, _)))
            | (Value::Index (_), Type::Array(box Type::Index,  (1, _)))
            | (Value::Scalar(_), Type::Array(box Type::Scalar, (1, _)))
            | (Value::Coords(_), Type::Array(box Type::Coords, (1, _)))
            | (Value::Color (_), Type::Array(box Type::Color,  (1, _)))
            | (Value::Tempo (_), Type::Array(box Type::Tempo,  (1, _)))
                => Some(Value::Array(vec![self])),

            // any -> str
            (_, Type::Text) => Some(Value::Text(self.to_string())),

            // str -> any
            (Value::Text(s), Type::Void  ) => s.is_empty().then_some(Value::Void),
            (Value::Text(s), Type::Cmd   ) => s.parse().ok().map(Value::Cmd   ),
            (Value::Text(s), Type::Vertex) => data.graph.find_vert(&s).map(Value::Vertex),
            (Value::Text(s), Type::Bool  ) => s.parse().ok().map(Value::Bool  ),
            (Value::Text(s), Type::Index ) => s.parse().ok().map(Value::Index ),
            (Value::Text(s), Type::Scalar) => s.parse().ok().map(Value::Scalar),
            (Value::Text(s), Type::Coords) => s.parse().ok().map(Value::Coords),
            (Value::Text(s), Type::Color ) => s.parse().ok().map(Value::Color ),
            (Value::Text(s), Type::Tempo ) => s.parse().ok().map(Value::Tempo ),

            _ => None,
        }
    }
}



// #[derive(Clone, PartialEq, Eq, Hash)]
// pub enum ArgType {
//     Simple(SimpleArgType),
//     Complex(ComplexArgType),
// }

// #[derive(Clone, PartialEq, Eq, Hash)]
// pub struct TupleType(Vec<ArgType>);

// #[derive(Clone, PartialEq, Eq, Hash)]
// pub enum ArrayType {
//     Simple(SimpleArgType),
//     Complex(Box<ComplexArgType>),
// }

// #[derive(Clone)]
// pub struct ArgTuple(Vec<Argument>);

// #[derive(Clone)]
// pub enum ArgArray {
//     Simple(SimpleArgArray),
//     Complex(ComplexArgArray),
// }

// #[derive(Clone)]
// pub enum Argument {
//     Simple(SimpleArgument),
//     Complex(ComplexArgument),
// }

// macro_rules! define_arguments {
//     (
//         Simple {
//             $($SimpleType:ident($SimpleRepr:ty)),+ $(,)?
//         }
//         Complex {
//             $($ComplexType:ident$(<$($ComplexGenerics:ty),+ $(,)?>)?($ComplexRepr:ty)),+ $(,)?
//         }
//     ) => {
//         #[derive(Clone, Copy, PartialEq, Eq, Hash)]
//         pub enum SimpleArgType {
//             $($SimpleType,)+
//         }

//         #[derive(Clone, PartialEq, Eq, Hash)]
//         pub enum ComplexArgType {
//             $($ComplexType$(($($ComplexGenerics),+))?,)+
//         }

//         #[derive(Clone)]
//         pub enum SimpleArgArray {
//             $($SimpleType(Vec<$SimpleRepr>),)+
//         }

//         #[derive(Clone)]
//         pub enum ComplexArgArray {
//             $($ComplexType(Vec<$ComplexRepr>),)+
//         }

//         #[derive(Clone)]
//         pub enum SimpleArgument {
//             $($SimpleType($SimpleRepr),)+
//         }

//         #[derive(Clone)]
//         pub enum ComplexArgument {
//             $($ComplexType($ComplexRepr),)+
//         }
//     };
// }

// define_arguments!{
//     Simple {
//         Text(String),
//         Cmd(Cmd),
//         Vertex(String),
//         Bool(bool),
//         Index(usize),
//         Scalar(f32),
//         Coords(Coords),
//         Color(RichColor),
//         Tempo(Tempo),
//     }
//     Complex {
//         Tuple<Vec<ArgType>>(ArgTuple),
//         Array<ArrayType, (usize, Option<usize>)>(ArgArray),
//     }
// }

// impl std::fmt::Display for SimpleArgument {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Text  (x) => x.fmt(f),
//             Self::Cmd   (x) => x.fmt(f),
//             Self::Vertex(x) => x.fmt(f),
//             Self::Bool  (x) => x.fmt(f),
//             Self::Index (x) => x.fmt(f),
//             Self::Scalar(x) => x.fmt(f),
//             Self::Coords(x) => x.fmt(f),
//             Self::Color (x) => x.fmt(f),
//             Self::Tempo (x) => x.fmt(f),
//         }
//     }
// }

// impl std::fmt::Display for ArgTuple {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_str(&self.0.iter()
//             .map(ToString::to_string)
//             .collect::<Vec<String>>()
//             .join(" ")
//         )
//     }
// }

// impl std::fmt::Display for SimpleArgArray {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_str(&match self {
//             Self::Text  (arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//             Self::Cmd   (arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//             Self::Vertex(arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//             Self::Bool  (arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//             Self::Index (arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//             Self::Scalar(arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//             Self::Coords(arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//             Self::Color (arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//             Self::Tempo (arr) => arr.iter().map(ToString::to_string).collect::<Vec<String>>(),
//         }.join(" "))
//     }
// }

// impl std::fmt::Display for ComplexArgArray {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_str(&match self {
//             Self::Tuple(arr) => arr.iter()
//                 .flat_map(|inner| inner.0.iter())
//                 .map(ToString::to_string)
//                 .collect::<Vec<String>>(),

//             Self::Array(arr) => arr.iter()
//                 .map(ToString::to_string)
//                 .collect::<Vec<String>>(),
//         }.join(" "))
//     }
// }

// impl std::fmt::Display for ArgArray {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Simple(inner) => inner.fmt(f),
//             Self::Complex(inner) => inner.fmt(f),
//         }
//     }
// }

// impl std::fmt::Display for ComplexArgument {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Tuple(inner) => inner.fmt(f),
//             Self::Array(inner) => inner.fmt(f),
//         }
//     }
// }

// impl std::fmt::Display for Argument {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Simple(inner) => inner.fmt(f),
//             Self::Complex(inner) => inner.fmt(f),
//         }
//     }
// }

// impl SimpleArgument {
//     pub const fn stored_type(&self) -> SimpleArgType {
//         match self {
//             Self::Text  (_) => SimpleArgType::Text,
//             Self::Cmd   (_) => SimpleArgType::Cmd,
//             Self::Vertex(_) => SimpleArgType::Vertex,
//             Self::Bool  (_) => SimpleArgType::Bool,
//             Self::Index (_) => SimpleArgType::Index,
//             Self::Scalar(_) => SimpleArgType::Scalar,
//             Self::Coords(_) => SimpleArgType::Coords,
//             Self::Color (_) => SimpleArgType::Color,
//             Self::Tempo (_) => SimpleArgType::Tempo,
//         }
//     }
// }

// impl ComplexArgument {
//     pub fn stored_type(&self) -> ComplexArgType {
//         match self {
//             Self::Tuple(arr) => {
//                 let list = arr.iter().map(Argument::stored_type).collect();
//                 ComplexArgType::Tuple(list)
//             }
//             Self::Array(arr) => {
//                 let (len, ty) = match arr {
//                     ArgArray::Simple(inner) => inner,
//                     ArgArray::Complex(inner) => todo!(),
//                 };
//                 ComplexArgType::Array(ty, (len, Some(len)))
//             }
//         }
//     }
// }

// impl Argument {
//     pub fn stored_type(&self) -> ArgType {
//         match self {
//             Self::Simple(inner) => ArgType::Simple(inner.stored_type()),
//             Self::Complex(inner) => ArgType::Complex(inner.stored_type()),
//         }
//     }

//     pub fn try_coerce(self, into: ArgType) -> Option<Self> {
//         match (into, self) {
//             | (ArgType::Text,   Self::Text  (_))
//             | (ArgType::Cmd,    Self::Cmd   (_))
//             | (ArgType::Vertex, Self::Vertex(_))
//             | (ArgType::Bool,   Self::Bool  (_))
//             | (ArgType::Index,  Self::Index (_))
//             | (ArgType::Scalar, Self::Scalar(_))
//             | (ArgType::Coords, Self::Coords(_))
//             | (ArgType::Color,  Self::Color (_))
//             | (ArgType::Tempo,  Self::Tempo (_))
//                 => Some(self),

//             (ArgType::Tuple(expect), Self::Tuple(actual)) =>
//                 if expect.len() == actual.len() {
//                     if expect.into_iter().zip(actual.into_iter()).all(|(expect, actual)| matches!()) {

//                     } else {

//                     }
//                     actual.into_iter().zip(expect.into_iter())
//                         .map(|(item, into)| item.try_coerce(into))
//                         .collect::<>()
//                 } else {
//                     None
//                 },

//             (ArgType::Array(expect_type, expect_min, expect_max), Self::Array(actual)) => Some(self),

//             (ArgType::Text, _) => Some(Argument::Text(self.to_string())),
//             (ArgType::Cmd, _) => ,
//             (ArgType::Vertex, _) => todo!(),
//             (ArgType::Bool, _) => todo!(),
//             (ArgType::Index, _) => todo!(),
//             (ArgType::Scalar, _) => todo!(),
//             (ArgType::Coords, _) => todo!(),
//             (ArgType::Color, _) => todo!(),
//             (ArgType::Tempo, _) => todo!(),
//             (ArgType::Tuple(arg_types), _) => todo!(),
//             (ArgType::Array(arg_type, _, _), _) => todo!(),
//         }
//     }
// }
