use std::{collections::VecDeque, path::Path};
use raylib::prelude::*;

type VertexID = u16;

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

impl Vertex {
    pub fn new(id: impl ToString, x: impl AsF32, y: impl AsF32) -> Self {
        Self {
            id: id.to_string(),
            pos: rvec2(x, y),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub id: Option<String>,
    pub adj: [VertexID; 2],
    pub weight: f32,
}

impl std::fmt::Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(id) = &self.id {
            write!(f, "{id}")
        } else {
            write!(f, "{}{}", self.adj[0], self.adj[1])
        }
    }
}

pub trait VertexIndex {
    fn vertex_index(&self, graph: &WeightedGraph) -> Option<VertexID>;
}
impl VertexIndex for VertexID {
    fn vertex_index(&self, _graph: &WeightedGraph) -> Option<VertexID> {
        Some(*self)
    }
}
impl VertexIndex for str {
    fn vertex_index(&self, graph: &WeightedGraph) -> Option<VertexID> {
        graph.verts.iter().position(|v| &v.id == self)?.try_into().ok()
    }
}

#[derive(Debug, Clone)]
pub struct WeightedGraph {
    pub verts: Vec<Vertex>,
    pub edges: Vec<Edge>,
}

impl WeightedGraph {
    pub fn load(_path: &Path) -> Option<Self> {
        todo!()
    }

    pub fn load_from_memory(verts_str: &str, edges_str: &str) -> Option<Self> {
        let verts = verts_str.split(';')
            .filter(|item| !item.trim().is_empty())
            .map(|item| {
                let (id_str, pos_str) = item.trim().split_once('=')?;
                let (x_str, y_str) = pos_str.split_once(',')?;
                let (x_str, y_str) = (x_str.trim(), y_str.trim());

                Some(Vertex {
                    id: id_str.to_string(),
                    pos: Vector2 {
                        x: x_str.parse().ok()?,
                        y: y_str.parse().ok()?,
                    },
                })
            })
            .collect::<Option<Vec<Vertex>>>()?;

        let edges = edges_str.split(';')
            .filter(|item| !item.trim().is_empty())
            .map(|item| {
                let (id_str, data_str) = item.trim().split_once('=')?;
                let (adj_str, weight_str) = data_str.split_once(':')?;
                let (id_a_str, id_b_str) = adj_str.split_once("--")?;
                let (id_a_str, id_b_str) = (id_a_str.trim(), id_b_str.trim());
                let id_a = verts.iter().position(|v| &v.id == id_a_str)?.try_into().ok()?;
                let id_b = verts.iter().position(|v| &v.id == id_b_str)?.try_into().ok()?;
                let mut id = id_str.trim();
                if id == "_" { id = ""; }
                Some(Edge {
                    id: (!id.is_empty()).then(|| id.to_string()),
                    adj: [id_a, id_b],
                    weight: weight_str.trim().parse().ok()?,
                })
            })
            .collect::<Option<Vec<Edge>>>()?;

        Some(Self { verts, edges })
    }

    pub fn gen_route(&self, start: VertexID, targets: impl IntoIterator<Item = VertexID>) -> Vec<VertexID> {
        let mut targets = Vec::from_iter(targets);
        let mut result = Vec::with_capacity(targets.len() + 1);
        let mut root = start;
        let mut distance_to_root: Box<[Option<(f32, Option<VertexID>)>]> = vec![const { None }; self.verts.len()].into_boxed_slice();
        let mut queue: VecDeque<VertexID> = VecDeque::new();
        while !targets.is_empty() {
            result.push(root);
            queue.clear();
            queue.push_back(root);
            distance_to_root.fill(const { None });
            distance_to_root[root as usize] = Some((0.0, None));
            while let Some(vertex) = queue.pop_front() {
                for edge in &self.edges {
                    let [a, b] = edge.adj;
                    let (src, dst) =
                        if a == vertex {
                            (a, b)
                        } else if b == vertex {
                            (b, a)
                        } else {
                            continue;
                        };
                    if distance_to_root[dst as usize].is_some() {
                        queue.push_back(dst);
                        println!("pushed vertex {dst} to queue");
                    }
                    let distance = distance_to_root[src as usize].expect("src must have been visited if it is queued").0 + edge.weight;
                    println!("vertex {root}'s distance to vertex {dst} (through vertex {src}) is {distance}");
                    if distance_to_root[dst as usize].is_none_or(|(d, _)| distance < d) {
                        distance_to_root[dst as usize] = Some((distance, Some(src)));
                    }
                    println!("vertex {}'s");
                }
            }
            let nearest_target = targets.iter().enumerate()
                .map(|(n, &v)| (n, distance_to_root[v as usize]))
                .min_by(|(_, d1), (_, d2)| d1.partial_cmp(d2).expect("no distance should be nan"))
                .map(|(n, _)| n).expect("targets must have at least one element");
            root = targets.swap_remove(nearest_target);
        }
        result
    }

    pub fn add_edge(&mut self, id: Option<impl ToString>, a_id: impl VertexIndex, b_id: impl VertexIndex, weight: impl AsF32) -> Option<()> {
        self.edges.push(Edge {
            id: id.map(|x| x.to_string()),
            adj: [
                a_id.vertex_index(self)?,
                b_id.vertex_index(self)?,
            ],
            weight: weight.as_f32(),
        });
        Some(())
    }
}

macro_rules! make_graph {
    (
        verts: {$(
            $v_id:ident = $x:expr, $y:expr;
        )*}
        edges: {$(
            $([$e_id:ident])? $a:ident--$b:ident$( : $weight:expr)?;
        )*}
    ) => {{
        #[allow(nonstandard_style, unused)]
        #[derive(Clone, Copy)]
        enum VertexNames { $($v_id),* }
        const POSITIONS: [Vector2; [$($x),*].len()] = [$(Vector2::new($x as f32, $y as f32)),*];
        fn dist(a: VertexNames, b: VertexNames) -> f32 {
            POSITIONS[a as usize].distance_to(POSITIONS[b as usize])
        }
        use VertexNames::*;
        WeightedGraph {
            verts: vec![$(Vertex::new(stringify!($v_id), $x, $y)),*],
            edges: vec![$(
                Edge {
                    id: make_graph!(@then: $($e_id)?),
                    adj: [$a as VertexID, $b as VertexID],
                    weight: make_graph!(@then: $($weight as f32)?).unwrap_or_else(|| dist($a, $b)),
                },
            )*],
        }
    }};

    (@then: $($tokens:tt)+) => { Some($($tokens)+) };
    (@then:) => { None };
}

fn main() {
    let window_width = 1000;
    let window_height = 1000;
    let (mut rl, thread) = init()
        .size(window_width, window_height)
        .title("Traversal")
        .build();

    let graph = make_graph!(
        verts: {
            // Satellites
            A =    0.0,    0.0; // Alpha
            B = -100.0, -200.0; // Bravo
            C =    0.0, -200.0; // Charlie
            D =  100.0, -200.0; // Delta
            E =  200.0, -100.0; // Echo
            F =  200.0,    0.0; // Foxtrot
            G =  200.0,  100.0; // Golf
            H =  100.0,  200.0; // Hotel
            I =    0.0,  200.0; // India
            J = -100.0,  200.0; // Juliett
            K = -200.0,  100.0; // Kilo
            L = -200.0,    0.0; // Lima
            M = -200.0, -100.0; // Mike
            N = -300.0, -300.0; // November
            O =  300.0, -300.0; // Oscar
            P =  300.0,  300.0; // Papa
            Q = -300.0,  300.0; // Quebec
            R = -500.0, -500.0; // Romeo
            S =    0.0, -500.0; // Sierra
            T =  500.0, -500.0; // Tango
            U =  500.0,    0.0; // Uniform
            V =  500.0,  500.0; // Victor
            W =    0.0,  500.0; // Whiskey
            X = -500.0,  500.0; // Xray
            Y = -500.0,    0.0; // Yankee

            // Transformers
            TR1 =  400.0,  200.0; // Transformer 1
            TR2 = -540.0,  232.0; // Transformer 2
            TR3 = -400.0, -475.0; // Transformer 3

            // Bridges
            brA1 =  -76.9,   11.7; // Bridge Alpha East
            brA2 =  -51.0,   11.8; // Bridge Alpha West
            brQ1 = -223.2,  285.1; // Bridge Quebec East
            brQ2 = -197.3,  282.8; // Bridge Quebec West
            brX1 = -400.2,  497.7; // Bridge Xray East
            brX2 = -376.6,  508.7; // Bridge Xray West
            brE1 =  116.2, -121.9; // Bridge Echo East
            brE2 =  120.7,  -96.3; // Bridge Echo West
            brO1 =  274.2, -360.0; // Bridge Oscar East
            brO2 =  295.5, -345.0; // Bridge Oscar West
        }
        edges: {
            A -- E  ; A -- F  ; A -- G  ; A -- H  ; A -- I  ; A -- J  ; A -- O  ; A -- P  ; A -- W  ; A -- T  ; A -- U  ; A -- V  ; A -- TR1;
            E -- F  ; E -- G  ; E -- H  ; E -- I  ; E -- J  ; E -- O  ; E -- P  ; E -- W  ; E -- T  ; E -- U  ; E -- V  ; E -- TR1;
            F -- G  ; F -- H  ; F -- I  ; F -- J  ; F -- O  ; F -- P  ; F -- W  ; F -- T  ; F -- U  ; F -- V  ; F -- TR1;
            G -- H  ; G -- I  ; G -- J  ; G -- O  ; G -- P  ; G -- W  ; G -- T  ; G -- U  ; G -- V  ; G -- TR1;
            H -- I  ; H -- J  ; H -- O  ; H -- P  ; H -- W  ; H -- T  ; H -- U  ; H -- V  ; H -- TR1;
            I -- J  ; I -- O  ; I -- P  ; I -- W  ; I -- T  ; I -- U  ; I -- V  ; I -- TR1;
            J -- O  ; J -- P  ; J -- W  ; J -- T  ; J -- U  ; J -- V  ; J -- TR1;
            O -- P  ; O -- W  ; O -- T  ; O -- U  ; O -- V  ; O -- TR1;
            P -- W  ; P -- T  ; P -- U  ; P -- V  ; P -- TR1;
            W -- T  ; W -- U  ; W -- V  ; W -- TR1;
            T -- U  ; T -- V  ; T -- TR1;
            U -- V  ; U -- TR1;
            V -- TR1;
            B -- C  ; B -- D  ; B -- K  ; B -- L  ; B -- M  ; B -- N  ; B -- Q  ; B -- R  ; B -- S  ; B -- X  ; B -- Y  ; B -- TR2; B -- TR3;
            C -- D  ; C -- K  ; C -- L  ; C -- M  ; C -- N  ; C -- Q  ; C -- R  ; C -- S  ; C -- X  ; C -- Y  ; C -- TR2; C -- TR3;
            D -- K  ; D -- L  ; D -- M  ; D -- N  ; D -- Q  ; D -- R  ; D -- S  ; D -- X  ; D -- Y  ; D -- TR2; D -- TR3;
            K -- L  ; K -- M  ; K -- N  ; K -- Q  ; K -- R  ; K -- S  ; K -- X  ; K -- Y  ; K -- TR2; K -- TR3;
            L -- M  ; L -- N  ; L -- Q  ; L -- R  ; L -- S  ; L -- X  ; L -- Y  ; L -- TR2; L -- TR3;
            M -- N  ; M -- Q  ; M -- R  ; M -- S  ; M -- X  ; M -- Y  ; M -- TR2; M -- TR3;
            N -- Q  ; N -- R  ; N -- S  ; N -- X  ; N -- Y  ; N -- TR2; N -- TR3;
            Q -- R  ; Q -- S  ; Q -- X  ; Q -- Y  ; Q -- TR2; Q -- TR3;
            R -- S  ; R -- X  ; R -- Y  ; R -- TR2; R -- TR3;
            S -- X  ; S -- Y  ; S -- TR2; S -- TR3;
            X -- Y  ; X -- TR2; X -- TR3;
            Y -- TR2; Y -- TR3;
            TR2  -- TR3 ;
            brX1 -- brQ1;
            brQ1 -- brA1;
            brA1 -- brE1;
            brE1 -- brO1;
            brX2 -- brQ2;
            brQ2 -- brA2;
            brA2 -- brE2;
            brE2 -- brO2;
            A   -- brA2; A   -- brE2; A   -- brO2; A   -- brQ2; A   -- brX2;
            E   -- brA2; E   -- brE2; E   -- brO2; E   -- brQ2; E   -- brX2;
            F   -- brA2; F   -- brE2; F   -- brO2; F   -- brQ2; F   -- brX2;
            G   -- brA2; G   -- brE2; G   -- brO2; G   -- brQ2; G   -- brX2;
            H   -- brA2; H   -- brE2; H   -- brO2; H   -- brQ2; H   -- brX2;
            I   -- brA2; I   -- brE2; I   -- brO2; I   -- brQ2; I   -- brX2;
            J   -- brA2; J   -- brE2; J   -- brO2; J   -- brQ2; J   -- brX2;
            O   -- brA2; O   -- brE2; O   -- brO2; O   -- brQ2; O   -- brX2;
            P   -- brA2; P   -- brE2; P   -- brO2; P   -- brQ2; P   -- brX2;
            W   -- brA2; W   -- brE2; W   -- brO2; W   -- brQ2; W   -- brX2;
            T   -- brA2; T   -- brE2; T   -- brO2; T   -- brQ2; T   -- brX2;
            U   -- brA2; U   -- brE2; U   -- brO2; U   -- brQ2; U   -- brX2;
            V   -- brA2; V   -- brE2; V   -- brO2; V   -- brQ2; V   -- brX2;
            TR1 -- brA2; TR1 -- brE2; TR1 -- brO2; TR1 -- brQ2; TR1 -- brX2;
            B   -- brA1; B   -- brE1; B   -- brO1; B   -- brQ1; B   -- brX1;
            C   -- brA1; C   -- brE1; C   -- brO1; C   -- brQ1; C   -- brX1;
            D   -- brA1; D   -- brE1; D   -- brO1; D   -- brQ1; D   -- brX1;
            K   -- brA1; K   -- brE1; K   -- brO1; K   -- brQ1; K   -- brX1;
            L   -- brA1; L   -- brE1; L   -- brO1; L   -- brQ1; L   -- brX1;
            M   -- brA1; M   -- brE1; M   -- brO1; M   -- brQ1; M   -- brX1;
            N   -- brA1; N   -- brE1; N   -- brO1; N   -- brQ1; N   -- brX1;
            Q   -- brA1; Q   -- brE1; Q   -- brO1; Q   -- brQ1; Q   -- brX1;
            R   -- brA1; R   -- brE1; R   -- brO1; R   -- brQ1; R   -- brX1;
            S   -- brA1; S   -- brE1; S   -- brO1; S   -- brQ1; S   -- brX1;
            X   -- brA1; X   -- brE1; X   -- brO1; X   -- brQ1; X   -- brX1;
            Y   -- brA1; Y   -- brE1; Y   -- brO1; Y   -- brQ1; Y   -- brX1;
            TR2 -- brA1; TR2 -- brE1; TR2 -- brO1; TR2 -- brQ1; TR2 -- brX1;
            TR3 -- brA1; TR3 -- brE1; TR3 -- brO1; TR3 -- brQ1; TR3 -- brX1;
            brX1 -- brX2;
            brQ1 -- brQ2;
            brA1 -- brA2;
            brE1 -- brE2;
            brO1 -- brO2;
        }
    );

    let route = graph.gen_route(0, [5]);
    println!("{route:?}");

    let camera = Camera2D {
        offset: rvec2(window_width/2, window_height/2),
        target: Vector2::zero(),
        rotation: 0.0,
        zoom: 0.85,
    };

    while !rl.window_should_close() {
        let mut d = rl.begin_drawing(&thread);
        d.clear_background(Color::BLACK);

        let mut d = d.begin_mode2D(camera);

        for edge in &graph.edges {
            let [i, j] = edge.adj.map(usize::from);
            let p0 = graph.verts[i].pos;
            let p1 = graph.verts[j].pos;
            d.draw_line_v(p0, p1, Color::RED.alpha(0.25));
        }

        for vert in &graph.verts {
            d.draw_circle_v(vert.pos, 8.0, Color::RED);
        }
    }
}
