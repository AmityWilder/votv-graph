use std::path::Path;

use raylib::prelude::*;

#[derive(Debug, Clone)]
pub struct Vertex {
    pub id: String,
    pub pos: Vector2,
}

impl std::fmt::Display for Vertex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub id: String,
    pub adj: [u16; 2],
    pub weight: f32,
}

impl std::fmt::Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Debug, Clone)]
pub struct WeightedGraph {
    pub verts: Vec<Vertex>,
    pub edges: Vec<Edge>,
}

impl WeightedGraph {
    pub fn load(path: &Path) -> Option<Self> {
        todo!()
    }

    pub fn load_from_memory(verts_str: &str, edges_str: &str) -> Option<Self> {
        let verts = verts_str.split(';')
            .map(|item| {
                let (id_str, pos_str) = item.trim().split_once('=')?;
                let (x_str, y_str) = pos_str.split_once(',')?;
                Some(Vertex {
                    id: id_str.to_string(),
                    pos: Vector2 {
                        x: x_str.trim().parse().ok()?,
                        y: y_str.trim().parse().ok()?,
                    },
                })
            })
            .collect::<Option<Vec<Vertex>>>()?;

        let edges = edges_str.split(';')
            .map(|item| {
                let (id_str, data_str) = item.trim().split_once('=')?;
                let (adj_str, weight_str) = data_str.split_once(':')?;
                let (id_a_str, id_b_str) = adj_str.split_once('-')?;
                let (id_a_str, id_b_str) = (id_a_str.trim(), id_b_str.trim());
                let id_a = verts.iter().position(|v| &v.id == id_a_str)?.try_into().ok()?;
                let id_b = verts.iter().position(|v| &v.id == id_b_str)?.try_into().ok()?;
                Some(Edge {
                    id: id_str.trim().to_string(),
                    adj: [id_a, id_b],
                    weight: weight_str.trim().parse().ok()?,
                })
            })
            .collect::<Option<Vec<Edge>>>()?;

        Some(Self { verts, edges })
    }

    pub fn gen_route(&self, targets: &[usize]) -> Vec<usize> {
        todo!()
    }
}

fn main() {
    let (mut rl, thread) = init()
        .size(1280, 720)
        .title("Traversal")
        .build();

    let mut graph = WeightedGraph::load_from_memory(
r"a=
b
c",
r"",
    );

    while !rl.window_should_close() {
        let mut d = rl.begin_drawing(&thread);
        d.clear_background(Color::RAYWHITE);
        for edge in graph.edges {
            let p1 = edge.adj[0];
        }
        for vert in graph.verts {

        }
    }
}
