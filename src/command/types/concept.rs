#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SimpleType {
    Text,
    Cmd,
    Vertex,
    Bool,
    Index,
    Scalar,
    Coords,
    Color,
    Tempo,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleType {
    pub ts: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumType {
    pub var: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayType {
    pub t: Box<Type>,
    pub size: (usize, Option<usize>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompoundType {
    Tuple(TupleType),
    Enum(EnumType),
    Array(ArrayType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Simple(SimpleType),
    Compound(CompoundType),
}
