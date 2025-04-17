use std::{collections::VecDeque, time::{Duration, Instant}};
use raylib::prelude::*;

type VertexID = u16;

#[derive(Debug, Clone)]
pub struct Vertex {
    pub id: String,
    pub pos: Vector3,
}

impl std::fmt::Display for Vertex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl Vertex {
    pub fn new(id: impl ToString, x: impl AsF32, y: impl AsF32, z: impl AsF32) -> Self {
        Self {
            id: id.to_string(),
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

pub enum Phase {
    None,
    Edge {
        current: VertexID,
        i: usize,
    },
    Target {
        shortest_distance: f32,
        nearest_target: usize,
        i: usize,
    },
    Backtrack {
        parent: VertexID,
        insert_at: usize,
    },
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct Visit {
    distance: f32,
    parent: Option<VertexID>,
}
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct Adjacent {
    vertex: VertexID,
    weight: f32,
}

pub struct RouteGenerator<'a> {
    pub targets: Vec<VertexID>,
    pub result: Vec<VertexID>,
    pub root: VertexID,
    pub visited: Box<[Option<Visit>]>,
    pub queue: VecDeque<VertexID>,
    pub phase: Phase,
    pub graph: &'a WeightedGraph,
    pub adjacent: Vec<Vec<Adjacent>>,
    pub is_finished: bool,
}

impl<'a> RouteGenerator<'a> {
    pub fn new(graph: &'a WeightedGraph, start: VertexID, targets: impl IntoIterator<Item = VertexID>) -> Self {
        Self {
            targets: Vec::from_iter(targets),
            result: vec![start],
            root: start,
            visited: vec![const { None }; graph.verts.len()].into_boxed_slice(),
            queue: VecDeque::new(),
            phase: Phase::None,
            graph,
            adjacent: (0..graph.verts.len() as VertexID)
                .map(|v| graph.edges.iter()
                    .filter_map(|e|
                        if e.adj[0] == v {
                            Some(Adjacent { vertex: e.adj[1], weight: e.weight })
                        } else if e.adj[1] == v {
                            Some(Adjacent { vertex: e.adj[0], weight: e.weight })
                        } else { None }
                    ).collect()
                ).collect(),
            is_finished: false,
        }
    }

    pub fn step(&mut self) {
        debug_assert!(!self.is_finished, "do not continue finished route");
        match &mut self.phase {
            Phase::None => {
                println!("root set to vertex {}; targeting {:?}", self.root, self.targets);
                println!("  edge phase");
                self.queue.clear();
                self.visited.fill(const { None });
                self.visited[self.root as usize] = Some(Visit { distance: 0.0, parent: None });
                self.phase = Phase::Edge {
                    current: self.root,
                    i: 0,
                };
            }

            Phase::Edge { current, i } => {
                if let Some(&Adjacent { vertex, weight }) = self.adjacent[*current as usize].get(*i) {
                    if self.visited[vertex as usize].is_none() {
                        self.queue.push_back(vertex);
                        println!("      pushed vertex {vertex} to queue");
                        for &Adjacent { vertex, .. } in &self.adjacent[vertex as usize] {
                            if !self.queue.contains(&vertex) {
                                self.queue.push_back(vertex);
                                println!("      pushed vertex {vertex} to queue");
                            }
                        }
                    }
                    let distance = self.visited[*current as usize].expect("current must have been visited if it is queued").distance + weight;
                    println!("      vertex {vertex} is {distance} from root (vertex {}) through vertex {current}", self.root);
                    if self.visited[vertex as usize].is_none_or(|visit| distance < visit.distance) {
                        self.visited[vertex as usize] = Some(Visit { distance, parent: Some(*current) });
                        println!("        updating vertex {vertex} best route");
                    }
                    *i += 1;
                } else {
                    if let Some(next_vert) = self.queue.pop_front() {
                        *current = next_vert;
                        *i = 0;
                        println!("    looking at vertex {current}");
                    } else {
                        println!("  target phase");
                        self.phase = Phase::Target {
                            shortest_distance: f32::INFINITY,
                            nearest_target: usize::MAX,
                            i: 0,
                        };
                    }
                }
            }

            Phase::Target { shortest_distance, nearest_target, i } => {
                if *i < self.targets.len() {
                    let v = self.targets[*i];
                    if let Some(Visit { distance, .. }) = self.visited[v as usize] {
                        println!("    target {} (vertex {v}) is {distance} from root (vertex {})", *i, self.root);
                        if distance < *shortest_distance {
                            println!("      updating nearest target to {} (vertex {v})", *i);
                            *shortest_distance = distance;
                            *nearest_target = *i;
                        }
                        *i += 1;
                    }
                } else {
                    assert!(*nearest_target < self.targets.len());
                    self.root = self.targets.swap_remove(*nearest_target);
                    println!("    nearest target identified as vertex {}", self.root);
                    println!("  backtrack phase");
                    let insert_at = self.result.len();
                    self.result.push(self.root);
                    self.phase = Phase::Backtrack {
                        parent: self.root,
                        insert_at,
                    };
                }
            }

            Phase::Backtrack { parent, insert_at } => {
                if let Some(Visit { parent: Some(p), .. }) = &self.visited[*parent as usize] {
                    println!("    parent of  {}", self.result[*insert_at]);
                    self.result.insert(*insert_at, *parent);
                    *parent = *p;
                } else {
                    println!("    adding vertex {} to results", self.root);
                    println!("  edge phase");
                    self.queue.clear();
                    self.visited.fill(const { None });
                    if self.targets.is_empty() {
                        println!("final route: {:?}", self.result);
                        self.is_finished = true;
                    } else {
                        self.visited[self.root as usize] = Some(Visit { distance: 0.0, parent: None });
                        self.phase = Phase::Edge {
                            current: self.root,
                            i: 0,
                        };
                    }
                }
            }
        }
    }
}

macro_rules! define_verts {
    ($($v_id:ident = ($x:expr, $y:expr, $z:expr);)*) => {
        #[allow(nonstandard_style, unused)]
        #[derive(Clone, Copy)]
        pub enum VertexNames { $($v_id),* }
        impl VertexNames {
            pub const POSITIONS: [Vector2; [$($x),*].len()] = [$(Vector2::new($x as f32, $y as f32)),*];
            pub fn distance_to(self, other: VertexNames) -> f32 {
                Self::POSITIONS[self as usize].distance_to(Self::POSITIONS[other as usize])
            }
            pub const fn id(self) -> VertexID {
                self as VertexID
            }
            pub fn verts() -> Vec<Vertex> {
                vec![$(Vertex::new(stringify!($v_id), $x, $y, $z)),*]
            }
        }
        #[allow(unused)]
        use VertexNames::{$($v_id),*};
    };
}

macro_rules! define_edges {
    ($($a:ident--$b:ident),* $(,)?) => {
        vec![$(
            Edge {
                id: None,
                adj: [$a.id(), $b.id()],
                weight: $a.distance_to($b),
            },
        )*]
    };
}

fn main() {
    let window_width = 1000;
    let window_height = 1000;
    let (mut rl, thread) = init()
        .size(window_width, window_height)
        .title("Traversal")
        .build();

    define_verts!{
        // Satellites
        A = (   0.0, 0.0,    0.0); // Alpha
        B = (-100.0, 0.0, -200.0); // Bravo
        C = (   0.0, 0.0, -200.0); // Charlie
        D = ( 100.0, 0.0, -200.0); // Delta
        E = ( 200.0, 0.0, -100.0); // Echo
        F = ( 200.0, 0.0,    0.0); // Foxtrot
        G = ( 200.0, 0.0,  100.0); // Golf
        H = ( 100.0, 0.0,  200.0); // Hotel
        I = (   0.0, 0.0,  200.0); // India
        J = (-100.0, 0.0,  200.0); // Juliett
        K = (-200.0, 0.0,  100.0); // Kilo
        L = (-200.0, 0.0,    0.0); // Lima
        M = (-200.0, 0.0, -100.0); // Mike
        N = (-300.0, 0.0, -300.0); // November
        O = ( 300.0, 0.0, -300.0); // Oscar
        P = ( 300.0, 0.0,  300.0); // Papa
        Q = (-300.0, 0.0,  300.0); // Quebec
        R = (-500.0, 0.0, -500.0); // Romeo
        S = (   0.0, 0.0, -500.0); // Sierra
        T = ( 500.0, 0.0, -500.0); // Tango
        U = ( 500.0, 0.0,    0.0); // Uniform
        V = ( 500.0, 0.0,  500.0); // Victor
        W = (   0.0, 0.0,  500.0); // Whiskey
        X = (-500.0, 0.0,  500.0); // Xray
        Y = (-500.0, 0.0,    0.0); // Yankee

        // Transformers
        TR1 = ( 400.0, 0.0,  200.0); // Transformer 1
        TR2 = (-540.0, 0.0,  232.0); // Transformer 2
        TR3 = (-400.0, 0.0, -475.0); // Transformer 3

        // Bridges
        brA1 = ( -76.9, 0.0,   11.7); // Bridge Alpha East
        brA2 = ( -51.0, 0.0,   11.8); // Bridge Alpha West
        brQ1 = (-223.2, 0.0,  285.1); // Bridge Quebec East
        brQ2 = (-197.3, 0.0,  282.8); // Bridge Quebec West
        brX1 = (-400.2, 0.0,  497.7); // Bridge Xray East
        brX2 = (-376.6, 0.0,  508.7); // Bridge Xray West
        brE1 = ( 116.2, 0.0, -121.9); // Bridge Echo East
        brE2 = ( 120.7, 0.0,  -96.3); // Bridge Echo West
        brO1 = ( 274.2, 0.0, -360.0); // Bridge Oscar East
        brO2 = ( 295.5, 0.0, -345.0); // Bridge Oscar West
    }

    let edges = define_edges!(
        A -- E,   A -- F,   A -- G,   A -- H,   A -- I,   A -- J,   A -- O,   A -- P, /*A -- W,   A -- T,   A -- U,   A -- V,   A -- TR1,*/
        E -- F, /*E -- G,*/ E -- H,   E -- I,   E -- J,   E -- O,   E -- P, /*E -- W,*/ E -- T,   E -- U,   E -- V,   E -- TR1,
        F -- G,   F -- H,   F -- I,   F -- J,   F -- O,   F -- P,   F -- W,   F -- T,   F -- U,   F -- V,   F -- TR1,
        G -- H,   G -- I,   G -- J,   G -- O,   G -- P,   G -- W,   G -- T,   G -- U,   G -- V,   G -- TR1,
        H -- I, /*H -- J,*/ H -- O,   H -- P,   H -- W,   H -- T,   H -- U,   H -- V,   H -- TR1,
        I -- J,   I -- O,   I -- P,   I -- W,   I -- T,   I -- U,   I -- V, /*I -- TR1,*/
        J -- O,   J -- P,   J -- W,   J -- T, /*J -- U,*/ J -- V, /*J -- TR1,*/
        O -- P,   O -- W,   O -- T,   O -- U,   O -- V,   O -- TR1,
        P -- W,   P -- T,   P -- U,   P -- V,   P -- TR1,
      /*W -- T,*/ W -- U,   W -- V,   W -- TR1,
        T -- U, /*T -- V,*/ T -- TR1,
        U -- V,   U -- TR1,
        V -- TR1,
        B -- C, /*B -- D,*/ B -- K,   B -- L,   B -- M,   B -- N,   B -- Q,   B -- R,   B -- S,   B -- X,   B -- Y,   B -- TR2, B -- TR3,
        C -- D,   C -- K,   C -- L,   C -- M,   C -- N,   C -- Q,   C -- R,   C -- S,   C -- X,   C -- Y,   C -- TR2, C -- TR3,
        D -- K,   D -- L,   D -- M,   D -- N,   D -- Q,   D -- R,   D -- S,   D -- X, /*D -- Y,*/ D -- TR2, D -- TR3,
        K -- L, /*K -- M,*/ K -- N,   K -- Q,   K -- R, /*K -- S,*/ K -- X,   K -- Y,   K -- TR2, K -- TR3,
        L -- M,   L -- N,   L -- Q,   L -- R,   L -- S,   L -- X,   L -- Y,   L -- TR2, L -- TR3,
        M -- N,   M -- Q,   M -- R,   M -- S,   M -- X,   M -- Y,   M -- TR2, M -- TR3,
        N -- Q,   N -- R,   N -- S,   N -- X,   N -- Y,   N -- TR2, N -- TR3,
        Q -- R,   Q -- S,   Q -- X,   Q -- Y,   Q -- TR2, Q -- TR3,
        R -- S, /*R -- X,*/ R -- Y,   R -- TR2, R -- TR3,
      /*S -- X,*/ S -- Y,   S -- TR2, S -- TR3,
        X -- Y,   X -- TR2, X -- TR3,
        Y -- TR2, Y -- TR3,
        TR2 -- TR3,
        brX1 -- brQ1,
        brQ1 -- brA1,
        brA1 -- brE1,
        brE1 -- brO1,
        brX2 -- brQ2,
        brQ2 -- brA2,
        brA2 -- brE2,
        brE2 -- brO2,
        A -- brA2, A -- brE2, A -- brO2, A -- brQ2,
        E -- brA2, E -- brE2, E -- brO2, E -- brQ2, E -- brX2,
        F -- brA2, F -- brE2, F -- brO2, F -- brQ2, F -- brX2,
        G -- brA2, G -- brE2, G -- brO2, G -- brQ2, G -- brX2,
        H -- brA2, H -- brE2, H -- brO2, H -- brQ2, H -- brX2,
        I -- brA2, I -- brE2, I -- brO2, I -- brQ2, I -- brX2,
        J -- brA2, J -- brE2, J -- brO2, J -- brQ2, J -- brX2,
        O -- brA2, O -- brE2, O -- brO2, O -- brQ2, O -- brX2,
        P -- brA2, P -- brE2, P -- brO2, P -- brQ2, P -- brX2,
        W -- brA2, W -- brE2, W -- brO2, W -- brQ2, W -- brX2,
        T -- brA2, T -- brE2, T -- brO2, T -- brQ2, T -- brX2,
        U -- brA2, U -- brE2, U -- brO2, U -- brQ2, U -- brX2,
        V -- brA2, V -- brE2, V -- brO2, V -- brQ2, V -- brX2,
        TR1 -- brA2, TR1 -- brE2, TR1 -- brO2, TR1 -- brQ2, TR1 -- brX2,
        B -- brA1, B -- brE1, B -- brO1, B -- brQ1, B -- brX1,
        C -- brA1, C -- brE1, C -- brO1, C -- brQ1, C -- brX1,
        D -- brA1, D -- brE1, D -- brO1, D -- brQ1, D -- brX1,
        K -- brA1, K -- brE1, K -- brO1, K -- brQ1, K -- brX1,
        L -- brA1, L -- brE1, L -- brO1, L -- brQ1, L -- brX1,
        M -- brA1, M -- brE1, M -- brO1, M -- brQ1, M -- brX1,
        N -- brA1, N -- brE1, N -- brO1, N -- brQ1, N -- brX1,
        Q -- brA1, Q -- brE1, Q -- brO1, Q -- brQ1, Q -- brX1,
        R -- brA1, R -- brE1, R -- brO1, R -- brQ1, R -- brX1,
        S -- brA1, S -- brE1, S -- brO1, S -- brQ1, S -- brX1,
        X -- brA1, X -- brE1, X -- brO1, X -- brQ1, X -- brX1,
        Y -- brA1, Y -- brE1, Y -- brO1, Y -- brQ1, Y -- brX1,
        TR2 -- brA1, TR2 -- brE1, TR2 -- brO1, TR2 -- brQ1, TR2 -- brX1,
        TR3 -- brA1, TR3 -- brE1, TR3 -- brO1, TR3 -- brQ1, TR3 -- brX1,
        brX1 -- brX2,
        brQ1 -- brQ2,
        brA1 -- brA2,
        brE1 -- brE2,
        brO1 -- brO2,
    );

    let graph = WeightedGraph {
        verts: VertexNames::verts(),
        edges,
    };

    let mut route = RouteGenerator::new(&graph, A.id(), [N.id(), TR3.id()]);
    // let mut last_route_step = Instant::now();

    let mut camera = Camera3D::perspective(Vector3::new(0.0, 1000.0, 0.0), Vector3::zero(), Vector3::new(0.0, 0.0, -1.0), 70.0);

    while !rl.window_should_close() {
        let speed = camera.position.distance_to(camera.target)/1000.0;
        let north = (rl.is_key_down(KeyboardKey::KEY_W) as i8 - rl.is_key_down(KeyboardKey::KEY_S) as i8) as f32*speed;
        let east  = (rl.is_key_down(KeyboardKey::KEY_D) as i8 - rl.is_key_down(KeyboardKey::KEY_A) as i8) as f32*speed;
        let up    = (rl.is_key_down(KeyboardKey::KEY_Q) as i8 - rl.is_key_down(KeyboardKey::KEY_E) as i8) as f32*speed;
        let pitch = (rl.is_key_down(KeyboardKey::KEY_R) as i8 - rl.is_key_down(KeyboardKey::KEY_F) as i8) as f32*speed;
        let yaw   = (rl.is_key_down(KeyboardKey::KEY_X) as i8 - rl.is_key_down(KeyboardKey::KEY_Z) as i8) as f32*speed;
        let pan = Vector3::new(east, 0.0, -north);
        let zoom = Vector3::new(yaw, up, -pitch);
        camera.position += pan + zoom;
        camera.target += pan;

        if !route.is_finished {
            // if last_route_step.elapsed() >= Duration::from_secs_f32(0.1) {
                // last_route_step = Instant::now();
                route.step();
            // }
        }

        let route = &route;

        let mut d = rl.begin_drawing(&thread);
        d.clear_background(Color::BLACK);

        {
            let mut d = d.begin_mode3D(camera);

            for edge in &graph.edges {
                let [i, j] = edge.adj.map(usize::from);
                let p0 = graph.verts[i].pos;
                let p1 = graph.verts[j].pos;
                d.draw_line_3D(p0, p1, Color::RED.alpha(0.25));
            }

            for (v, vert) in graph.verts.iter().enumerate().map(|(v, vert)| (v as VertexID, vert)) {
                let distance_from_target = vert.pos - camera.target;
                let color = loop {
                    if let Phase::Edge { current, i } = &route.phase {
                        if &v == current {
                            break Color::SKYBLUE;
                        } else if route.adjacent[*current as usize].get(*i).is_some_and(|x| &v == &x.vertex) {
                            break Color::ORANGE;
                        }
                    }
                    if v == route.root {
                        break Color::BLUE;
                    } else if route.result.contains(&v) {
                        break Color::BLUEVIOLET;
                    } else if route.targets.contains(&v) {
                        break Color::GREEN;
                    } else {
                        let dist_sqr = distance_from_target.dot(distance_from_target);
                        if dist_sqr <= 8.0*8.0 {
                            break Color::new(255, 128, 128, 255);
                        } else {
                            break Color::RED;
                        }
                    }
                };
                d.draw_sphere(vert.pos, 8.0, color);
                if let Some(Visit { parent: Some(p), .. }) = route.visited[v as usize] {
                    d.draw_line_3D(graph.verts[p as usize].pos, vert.pos, Color::BLUE);
                }
            }

            for pair in route.result.windows(2) {
                let [a, b] = pair else { panic!("window(2) should always create 2 elements") };
                d.draw_capsule(graph.verts[*a as usize].pos, graph.verts[*b as usize].pos, 2.0, 16, 16, Color::BLUEVIOLET);
            }

            d.draw_line_3D(camera.target + Vector3::new(-5.0, 0.0,  0.0), camera.target + Vector3::new(5.0, 0.0, 0.0), Color::BLUEVIOLET);
            d.draw_line_3D(camera.target + Vector3::new( 0.0, 0.0, -5.0), camera.target + Vector3::new(0.0, 0.0, 5.0), Color::BLUEVIOLET);
        }

        for (v, vert) in graph.verts.iter().enumerate().map(|(v, vert)| (v as VertexID, vert)) {
            let pos = d.get_world_to_screen(vert.pos, camera);
            let text_width = d.measure_text(&vert.id, 10);
            d.draw_text(&vert.id, pos.x as i32 - text_width/2, pos.y as i32 - 5, 10, Color::WHITE);
            if let Some(Visit { distance, parent }) = route.visited[v as usize] {
                let parent_text = parent.map_or("-", |p| &graph.verts[p as usize].id);
                let text = format!("{distance} ({parent_text})");
                d.draw_text(&text, pos.x as i32 + text_width/2 + 3, pos.y as i32 + 3, 10, Color::GRAY);
            }
        }
    }
}
