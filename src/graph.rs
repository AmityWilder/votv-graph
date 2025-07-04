use raylib::prelude::*;

pub type VertexID = u16;

#[derive(Debug, Clone)]
pub struct Vertex {
    pub id: String,
    pub alias: String,
    pub pos: Vector3,
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub adj: [VertexID; 2],
    pub weight: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Adjacent {
    pub vertex: VertexID,
    pub weight: f32,
}

#[derive(Debug, Clone)]
pub struct WeightedGraph {
    verts: Vec<Vertex>,
    edges: Vec<Edge>,
    adjacent: Vec<Vec<Adjacent>>,
}
impl WeightedGraph {
    pub fn new(verts: Vec<Vertex>, edges: Vec<Edge>) -> Self {
        Self {
            adjacent: (0..verts.len() as VertexID)
                .map(|v| edges.iter()
                    .filter_map(|e|
                        if e.adj[0] == v {
                            Some(Adjacent { vertex: e.adj[1], weight: e.weight })
                        } else if e.adj[1] == v {
                            Some(Adjacent { vertex: e.adj[0], weight: e.weight })
                        } else { None }
                    ).collect()
                ).collect(),
            verts,
            edges,
        }
    }

    pub fn vert(&self, id: VertexID) -> &Vertex {
        &self.verts[id as usize]
    }

    pub fn _vert_mut(&mut self, id: VertexID) -> &mut Vertex {
        &mut self.verts[id as usize]
    }

    pub fn verts(&self) -> &[Vertex] {
        &self.verts
    }

    pub fn edges(&self) -> &[Edge] {
        &self.edges
    }

    pub fn adjacent(&self, vertex: VertexID) -> &[Adjacent] {
        &self.adjacent[vertex as usize]
    }

    pub fn verts_iter(&self) -> impl ExactSizeIterator<Item = (VertexID, &Vertex)> + DoubleEndedIterator {
        self.verts.iter()
            .enumerate()
            .map(|(v, vert)| (VertexID::try_from(v).unwrap_or_else(|_| panic!("greater than {} vertices is not supported", VertexID::MAX)), vert))
    }

    pub fn find_vert(&self, id_or_alias: &str) -> Option<VertexID> {
        self.verts.iter()
            .position(|vert| vert.id.eq_ignore_ascii_case(id_or_alias) || vert.alias.eq_ignore_ascii_case(id_or_alias))
            .map(|v| VertexID::try_from(v).unwrap_or_else(|_| panic!("greater than {} vertices is not supported", VertexID::MAX)))
    }

    pub fn add_edge(&mut self, a: VertexID, b: VertexID) {
        let weight = self.verts[a as usize].pos.distance(self.verts[b as usize].pos);
        self.edges.push(Edge {
            adj: [a, b],
            weight,
        });
        self.adjacent[a as usize].push(Adjacent { vertex: b, weight });
        self.adjacent[b as usize].push(Adjacent { vertex: a, weight });
    }

    pub fn add_vertex(&mut self, id: impl ToString, alias: impl ToString, pos: Vector3) {
        self.verts.push(Vertex { id: id.to_string(), alias: alias.to_string(), pos });
        self.adjacent.push(Vec::new());
    }
}
