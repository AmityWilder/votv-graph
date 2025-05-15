use crate::graph::VertexID;
pub use crate::{command::{Cmd, ProgramData}, types::{Coords, RichColor, Tempo}};
use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct SizeHint {
    pub low: usize,
    pub high: Option<usize>,
}

impl SizeHint {
    #[inline]
    pub const fn contains(&self, n: usize) -> bool {
        self.low <= n && match self.high {
            Some(high) => n <= high,
            None => true,
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
    Tuple(Vec<Type>),
    Union(Vec<Type>),
    Array(Box<Type>, SizeHint),
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

            // [T; l<=n<=h] == [T; l..=h]
            Type::Array(t, size) => {
                if let Value::Array(items) = self {
                    size.contains(items.len()) &&
                        items.iter().all(|item| item.is_type(t))
                } else { false }
            }

            // (T, U, V) == (T, U, V)
            Type::Tuple(ts) => {
                if let Value::Array(items) = self {
                    ts.len() == items.len() &&
                        ts.iter().zip(items.iter()).all(|(t, item)| item.is_type(t))
                } else { false }
            }
        }
    }

    fn coerce_parse(s: &str, t: &Type, data: &ProgramData) -> Option<Value> {
        match t {
            // "" -> ()
            Type::Void => s.is_empty().then_some(Value::Void),

            // str -> str (should normally be handled by `coerce_to`, but just in case...)
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
                        break (t.is_none() && s.is_none()).then_some(Value::Array(items));
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
                    // "<T str> <T str> <T str> ..." -> [T; ..n..]
                    Some(Value::Array(items))
                } else {
                    None
                }
            }
        }
    }

    pub fn coerce_to(mut self, t: &Type, data: &ProgramData) -> Option<Value> {
        let new_value = if self.is_type(t) {
            // T -> T
            Some(self)
        } else if let Value::Array(items) = &mut self && matches!(t, Type::Void) && items.is_empty() {
            // [T; 0] -> ()
            Some(Value::Void)
        } else if let Type::Array(_, size) = t && size.contains(0) && matches!(self, Value::Void) {
            // () -> [T; 0..]
            Some(Value::Array(Vec::new()))
        } else if let Value::Array(items) = &mut self && let [item] = &items[..] && item.is_type(t) {
            // [T; 1] -> T
            Some(items.pop().expect("should be guarded by `[item] = &items[..]`"))
        } else if let Type::Array(t, size) = t && size.contains(1) && self.is_type(t) {
            // T -> [T; ..1..]
            Some(Value::Array(vec![self]))
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
