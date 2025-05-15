use raylib::prelude::*;

pub type VtxID = u16;

#[derive(Debug)]
pub enum VertexIDError {
    OutOfRange(std::num::TryFromIntError),
}

impl std::fmt::Display for VertexIDError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "greater than {} vertices is not supported", VertexID::MAX)
    }
}

impl std::error::Error for VertexIDError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::OutOfRange(e) => Some(e),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VertexID(VtxID);

impl std::fmt::Display for VertexID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl VertexID {
    pub const MAX: VtxID = VtxID::MAX;
    pub const MIN: VtxID = VtxID::MIN;
    pub const BITS: u32 = VtxID::BITS;

    pub fn from_usize(index: usize, _marker: &Vertex) -> Result<Self, VertexIDError> {
        match VtxID::try_from(index) {
            Ok(n) => Ok(Self(n)),
            Err(e) => Err(VertexIDError::OutOfRange(e)),
        }
    }

    pub const fn new(index: VtxID, _marker: &Vertex) -> Self {
        Self(index)
    }

    #[inline]
    const fn as_num(self) -> VtxID {
        self.0
    }

    #[inline]
    const fn as_idx(self) -> usize {
        self.0 as usize
    }

    #[inline]
    pub fn fmt(self, graph: &WeightedGraph, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&graph.vert(self).alias, f)
    }

    #[inline]
    pub fn to_string(self, graph: &WeightedGraph) -> String {
        graph.vert(self).alias.clone()
    }

    #[inline]
    pub fn as_str(self, graph: &WeightedGraph) -> &str {
        graph.vert(self).alias.as_str()
    }
}

#[derive(Debug, Clone)]
pub struct Vertex {
    pub id: String,
    pub alias: String,
    pub pos: Vector3,
}

#[derive(Debug, Clone, Copy)]
pub struct Edge {
    pub adj: [VertexID; 2],
    pub weight: f32,
}

#[derive(Debug, Clone, Copy)]
pub struct Adjacent {
    pub vertex: VertexID,
    pub weight: f32,
}

#[derive(Debug, Clone)]
pub struct WeightedGraph {
    verts_adj: Vec<(Vertex, Vec<Adjacent>)>,
    edges: Vec<Edge>,
}

impl WeightedGraph {
    pub fn new(verts: impl IntoIterator<Item = Vertex>, edges: Vec<Edge>) -> Self {
        Self {
            verts_adj: verts.into_iter()
                .enumerate()
                .map(|(v, vert)| {
                    let v = VertexID::from_usize(v, &vert).expect(&format!("greater than {} vertices is not supported", VertexID::MAX));
                    (vert, edges.iter()
                        .filter_map(|e| {
                            let [a, b] = e.adj;
                            Some(Adjacent {
                                vertex: (a == v).then_some(b)
                                    .or_else(|| (b == v).then_some(a))?,
                                weight: e.weight
                            })
                        })
                        .collect())
                })
                .collect(),
            edges,
        }
    }

    pub fn vert(&self, id: VertexID) -> &Vertex {
        &self.verts_adj[id.as_idx()].0
    }

    pub fn adjacent(&self, vertex: VertexID) -> &[Adjacent] {
        &self.verts_adj[vertex.as_idx()].1
    }

    pub fn adjacent_mut(&mut self, vertex: VertexID) -> &mut Vec<Adjacent> {
        &mut self.verts_adj[vertex.as_idx()].1
    }

    pub fn verts_iter(&self) -> impl ExactSizeIterator<Item = (VertexID, &Vertex)> + DoubleEndedIterator + Clone {
        self.verts_adj.iter()
            .enumerate()
            .map(|(v, (vert, _))| (VertexID::from_usize(v, vert).unwrap(), vert))
    }

    pub fn edges_iter(&self) -> impl ExactSizeIterator<Item = &Edge> + DoubleEndedIterator + Clone {
        self.edges.iter()
    }

    pub fn find_vert(&self, id_or_alias: &str) -> Option<VertexID> {
        self.verts_adj.iter()
            .position(|(vert, _)| vert.id.eq_ignore_ascii_case(id_or_alias) || vert.alias.eq_ignore_ascii_case(id_or_alias))
            .map(|v| VertexID::from_usize(v, &self.verts_adj[v].0).unwrap())
    }

    pub fn add_edge(&mut self, a: VertexID, b: VertexID) {
        let weight = self.vert(a).pos.distance_to(self.vert(b).pos);
        self.edges.push(Edge { adj: [a, b], weight });
        self.adjacent_mut(a).push(Adjacent { vertex: b, weight });
        self.adjacent_mut(b).push(Adjacent { vertex: a, weight });
    }

    pub fn add_vertex(&mut self, id: impl ToString, alias: impl ToString, pos: Vector3) {
        self.verts_adj.push((Vertex { id: id.to_string(), alias: alias.to_string(), pos }, Vec::new()));
    }

    #[inline]
    pub fn associate_verts<T: Clone>(&self, value: T) -> AssociateVerts<T> {
        AssociateVerts(
            std::iter::repeat_n(value, self.verts_adj.len())
                .collect::<Box<[T]>>()
        )
    }

    #[inline]
    pub fn associate_verts_with<T, F: FnMut((VertexID, &Vertex)) -> T>(&self, init: F) -> AssociateVerts<T> {
        AssociateVerts(
            self.verts_iter()
                .map(init)
                .collect::<Box<[T]>>()
        )
    }
}

pub struct AssociateVerts<T>(Box<[T]>);

impl<T> std::ops::Index<VertexID> for AssociateVerts<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: VertexID) -> &Self::Output {
        &self.0[index.as_idx()]
    }
}

impl<T> std::ops::IndexMut<VertexID> for AssociateVerts<T> {
    #[inline]
    fn index_mut(&mut self, index: VertexID) -> &mut Self::Output {
        &mut self.0[index.as_idx()]
    }
}

impl<T> std::ops::Deref for AssociateVerts<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T> std::ops::DerefMut for AssociateVerts<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}
