use raylib::prelude::*;

pub type VertexID = u16;

#[derive(Debug, Clone)]
pub struct Vertex {
    pub id: String,
    pub alias: String,
    pub pos: Vector3,
}

impl Vertex {
    pub fn new(id: impl ToString, alias: impl ToString, x: impl AsF32, y: impl AsF32, z: impl AsF32) -> Self {
        Self {
            id: id.to_string(),
            alias: alias.to_string(),
            pos: rvec3(x, y, z),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub id: Option<String>,
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

    pub fn vert_mut(&mut self, id: VertexID) -> &mut Vertex {
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
            .map(|(v, vert)| (VertexID::try_from(v).expect(&format!("greater than {} vertices is not supported", VertexID::MAX)), vert))
    }

    pub fn find_vert<'a>(&self, id_or_alias: &'a str) -> Result<VertexID, &'a str> {
        self.verts.iter()
            .position(|vert| vert.id.eq_ignore_ascii_case(id_or_alias) || vert.alias.eq_ignore_ascii_case(id_or_alias))
            .map(|v| VertexID::try_from(v).expect(&format!("greater than {} vertices is not supported", VertexID::MAX)))
            .ok_or(id_or_alias)
    }

    pub fn add_edge(&mut self, a: VertexID, b: VertexID) {
        let weight = self.verts[a as usize].pos.distance_to(self.verts[b as usize].pos);
        self.edges.push(Edge {
            id: None,
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

macro_rules! define_verts {
    ($name:ident: $($v_id:ident ($v_alias:ident) = ($x:expr, $y:expr, $z:expr);)*) => {
        #[allow(nonstandard_style, unused)]
        #[derive(Clone, Copy)]
        pub enum VertexNames { $($v_id),* }
        impl VertexNames {
            pub const POSITIONS: [Vector3; [$($x),*].len()] = [$(Vector3::new($x as f32, $y as f32, $z as f32)),*];
            pub fn distance_to(self, other: VertexNames) -> f32 {
                Self::POSITIONS[self as usize].distance_to(Self::POSITIONS[other as usize])
            }
            pub const fn id(self) -> $crate::graph::VertexID {
                self as $crate::graph::VertexID
            }
        }
        #[allow(unused)]
        use VertexNames::{$($v_id as $v_alias),*};
        let $name = vec![$($crate::graph::Vertex::new(stringify!($v_id), stringify!($v_alias), $x, $y, $z)),*];
    };
}

macro_rules! define_edges {
    ($name:ident: $($a:ident--$b:ident),* $(,)?) => {
        let $name = vec![$(
            $crate::graph::Edge {
                id: None,
                adj: [$a.id(), $b.id()],
                weight: $a.distance_to($b),
            },
        )*];
    };
}

pub(crate) use {define_verts, define_edges};
