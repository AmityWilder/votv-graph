pub use crate::{command::Cmd, types::{Coords, RichColor, Tempo}};

pub mod concept;
pub mod solid;

pub type Text = String;
pub type Vertex = String;
pub type Bool = bool;
pub type Index = usize;
pub type Scalar = f32;
pub type Color = RichColor;


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
