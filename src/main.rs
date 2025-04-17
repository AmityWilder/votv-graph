use std::collections::VecDeque;
// use std::time::{Duration, Instant};
use raylib::prelude::*;

type VertexID = u16;

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
        i: usize,
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

    pub fn step(&mut self, mut debug_msg: impl FnMut(std::fmt::Arguments<'_>)) {
        debug_assert!(!self.is_finished, "do not continue finished route");
        match &mut self.phase {
            Phase::None => {
                debug_msg(format_args!("root set to vertex {}; targeting {:?}", self.root, self.targets));
                debug_msg(format_args!("  edge phase"));
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
                        // println!("      pushed vertex {vertex} to queue");
                        for &Adjacent { vertex, .. } in &self.adjacent[vertex as usize] {
                            if !self.queue.contains(&vertex) {
                                self.queue.push_back(vertex);
                                // println!("      pushed vertex {vertex} to queue");
                            }
                        }
                    }
                    let distance = self.visited[*current as usize].expect("current must have been visited if it is queued").distance + weight;
                    if self.visited[vertex as usize].is_none_or(|visit| distance < visit.distance) {
                        self.visited[vertex as usize] = Some(Visit { distance, parent: Some(*current) });
                        debug_msg(format_args!("      vertex {vertex} is {distance} from root (vertex {}) through vertex {current}, new best", self.root));
                    }
                    *i += 1;
                } else {
                    if let Some(next_vert) = self.queue.pop_front() {
                        *current = next_vert;
                        *i = 0;
                        // debug_msg(format_args!("    looking at vertex {current}"));
                    } else {
                        debug_msg(format_args!("  target phase"));
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
                        debug_msg(format_args!("    target {} (vertex {v}) is {distance} from root (vertex {})", *i, self.root));
                        if distance < *shortest_distance {
                            debug_msg(format_args!("      updating nearest target to {} (vertex {v})", *i));
                            *shortest_distance = distance;
                            *nearest_target = *i;
                        }
                        *i += 1;
                    }
                } else {
                    assert!(*nearest_target < self.targets.len());
                    self.root = self.targets.swap_remove(*nearest_target);
                    debug_msg(format_args!("    nearest target identified as vertex {}", self.root));
                    debug_msg(format_args!("  backtrack phase"));
                    let insert_at = self.result.len();
                    self.phase = Phase::Backtrack {
                        parent: self.root,
                        insert_at,
                        i: 0,
                    };
                }
            }

            Phase::Backtrack { parent, insert_at, i } => {
                if let Some(Visit { parent: Some(p), .. }) = &self.visited[*parent as usize] {
                    self.result.insert(*insert_at, *parent);
                    *parent = *p;
                } else {
                    debug_msg(format_args!("    adding vertex {} to results", self.root));
                    debug_msg(format_args!("  edge phase"));
                    self.queue.clear();
                    self.visited.fill(const { None });
                    if self.targets.is_empty() {
                        debug_msg(format_args!("final route: {:?}", self.result));
                        self.is_finished = true;
                        let mut distance = 0.0;
                        self.visited[self.result[0] as usize] = Some(Visit { distance, parent: None });
                        if *i + 1 < self.result.len() {
                            let a = &self.result[*i];
                            let b = &self.result[*i + 1];
                            distance += self.adjacent[*a as usize].iter()
                                .find_map(|e| (&e.vertex == b).then_some(e.weight))
                                .expect("results should be adjacent");
                            self.visited[*b as usize] = Some(Visit { distance, parent: Some(*a) });
                            *i += 1;
                        }
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
    ($($v_id:ident ($v_alias:ident) = ($x:expr, $y:expr, $z:expr);)*) => {
        #[allow(nonstandard_style, unused)]
        #[derive(Clone, Copy)]
        pub enum VertexNames { $($v_id),* }
        impl VertexNames {
            pub const POSITIONS: [Vector3; [$($x),*].len()] = [$(Vector3::new($x as f32, $y as f32, $z as f32)),*];
            pub fn distance_to(self, other: VertexNames) -> f32 {
                Self::POSITIONS[self as usize].distance_to(Self::POSITIONS[other as usize])
            }
            pub const fn id(self) -> VertexID {
                self as VertexID
            }
            pub fn verts() -> Vec<Vertex> {
                vec![$(Vertex::new(stringify!($v_id), stringify!($v_alias), $x, $y, $z)),*]
            }
        }
        #[allow(unused)]
        use VertexNames::{$($v_id as $v_alias),*};
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
    let window_width = 640;
    let window_height = 640;
    let (mut rl, thread) = init()
        .size(window_width, window_height)
        .title("Traversal")
        .resizable()
        .build();

    define_verts!{
        // Satellites
        Alpha    (A) = (   0.0, 0.0,    0.0);
        Bravo    (B) = (-100.0, 0.0, -200.0);
        Charlie  (C) = (   0.0, 0.0, -200.0);
        Delta    (D) = ( 100.0, 0.0, -200.0);
        Echo     (E) = ( 200.0, 0.0, -100.0);
        Foxtrot  (F) = ( 200.0, 0.0,    0.0);
        Golf     (G) = ( 200.0, 0.0,  100.0);
        Hotel    (H) = ( 100.0, 0.0,  200.0);
        India    (I) = (   0.0, 0.0,  200.0);
        Juliett  (J) = (-100.0, 0.0,  200.0);
        Kilo     (K) = (-200.0, 0.0,  100.0);
        Lima     (L) = (-200.0, 0.0,    0.0);
        Mike     (M) = (-200.0, 0.0, -100.0);
        November (N) = (-300.0, 0.0, -300.0);
        Oscar    (O) = ( 300.0, 0.0, -300.0);
        Papa     (P) = ( 300.0, 0.0,  300.0);
        Quebec   (Q) = (-300.0, 0.0,  300.0);
        Romeo    (R) = (-500.0, 0.0, -500.0);
        Sierra   (S) = (   0.0, 0.0, -500.0);
        Tango    (T) = ( 500.0, 0.0, -500.0);
        Uniform  (U) = ( 500.0, 0.0,    0.0);
        Victor   (V) = ( 500.0, 0.0,  500.0);
        Whiskey  (W) = (   0.0, 0.0,  500.0);
        Xray     (X) = (-500.0, 0.0,  500.0);
        Yankee   (Y) = (-500.0, 0.0,    0.0);

        // Transformers
        TR1 (TR1) = ( 400.0, 0.0,  200.0); // Transformer 1
        TR2 (TR2) = (-540.0, 0.0,  232.0); // Transformer 2
        TR3 (TR3) = (-400.0, 0.0, -475.0); // Transformer 3

        // Bridges
        Bridge_Alpha_East  (brAe) = ( -76.9, 0.0,   11.7); // Bridge Alpha East
        Bridge_Alpha_West  (brAw) = ( -51.0, 0.0,   11.8); // Bridge Alpha West
        Bridge_Quebec_East (brQe) = (-223.2, 0.0,  285.1); // Bridge Quebec East
        Bridge_Quebec_West (brQw) = (-197.3, 0.0,  282.8); // Bridge Quebec West
        Bridge_Xray_East   (brXe) = (-400.2, 0.0,  497.7); // Bridge Xray East
        Bridge_Xray_West   (brXw) = (-376.6, 0.0,  508.7); // Bridge Xray West
        Bridge_Echo_East   (brEe) = ( 116.2, 0.0, -121.9); // Bridge Echo East
        Bridge_Echo_West   (brEw) = ( 120.7, 0.0,  -96.3); // Bridge Echo West
        Bridge_Oscar_East  (brOe) = ( 274.2, 0.0, -360.0); // Bridge Oscar East
        Bridge_Oscar_West  (brOw) = ( 295.5, 0.0, -345.0); // Bridge Oscar West
    }

    let edges = define_edges!(
        A -- E,   A -- F,   A -- G,   A -- H,   A -- I,   A -- J,   A -- P,
        E -- F,   E -- H,   E -- I,   E -- J,   E -- O,   E -- P,   E -- T,   E -- U,   E -- V,   E -- TR1,
        F -- G,   F -- H,   F -- I,   F -- J,   F -- O,   F -- P,   F -- W,   F -- T,   F -- U,   F -- V,   F -- TR1,
        G -- H,   G -- I,   G -- J,   G -- O,   G -- P,   G -- W,   G -- T,   G -- U,   G -- V,   G -- TR1,
        H -- I,   H -- O,   H -- P,   H -- W,   H -- T,   H -- U,   H -- V,   H -- TR1,
        I -- J,   I -- O,   I -- P,   I -- W,   I -- T,   I -- U,   I -- V,
        J -- O,   J -- P,   J -- W,   J -- T,   J -- V,
        O -- P,   O -- W,   O -- T,   O -- U,   O -- V,   O -- TR1,
        P -- W,   P -- T,   P -- U,   P -- V,   P -- TR1,
        W -- U,   W -- V,   W -- TR1,
        T -- U,   T -- TR1,
        U -- V,   U -- TR1,
        V -- TR1,
        B -- C,   B -- K,   B -- L,   B -- M,   B -- N,   B -- Q,   B -- R,   B -- S,   B -- X,   B -- Y,   B -- TR2, B -- TR3,
        C -- D,   C -- K,   C -- L,   C -- M,   C -- N,   C -- Q,   C -- R,   C -- S,   C -- X,   C -- Y,   C -- TR2, C -- TR3,
        D -- K,   D -- L,   D -- M,   D -- N,   D -- R,   D -- S,   D -- X,   D -- TR2, D -- TR3,
        K -- L,   K -- N,   K -- Q,   K -- R,   K -- X,   K -- Y,   K -- TR2, K -- TR3,
        L -- M,   L -- N,   L -- Q,   L -- R,   L -- S,   L -- X,   L -- Y,   L -- TR2, L -- TR3,
        M -- N,   M -- Q,   M -- R,   M -- S,   M -- X,   M -- Y,   M -- TR2, M -- TR3,
        N -- Q,   N -- R,   N -- S,   N -- X,   N -- Y,   N -- TR2, N -- TR3,
        Q -- R,   Q -- X,   Q -- Y,   Q -- TR2, Q -- TR3,
        R -- S,   R -- Y,   R -- TR2, R -- TR3,
        S -- Y,   S -- TR2, S -- TR3,
        X -- Y,   X -- TR2, X -- TR3,
        Y -- TR2, Y -- TR3,
        TR2 -- TR3,
        brXe -- brQe,
        brQe -- brAe,
        brAe -- brEe,
        brEe -- brOe,
        brXw -- brQw,
        brQw -- brAw,
        brAw -- brEw,
        brEw -- brOw,
        A -- brAw, A -- brEw, A -- brQw,
        E -- brAw, E -- brEw, E -- brOw, E -- brQw, E -- brXw,
        F -- brAw, F -- brEw, F -- brOw, F -- brQw, F -- brXw,
        G -- brAw, G -- brEw, G -- brOw, G -- brQw, G -- brXw,
        H -- brAw, H -- brEw, H -- brOw, H -- brQw, H -- brXw,
        I -- brAw, I -- brEw, I -- brOw, I -- brQw, I -- brXw,
        J -- brAw, J -- brEw, J -- brQw, J -- brXw,
        O -- brEw, O -- brOw, O -- brQw, O -- brXw,
        P -- brAw, P -- brEw, P -- brOw, P -- brQw, P -- brXw,
        W -- brAw, W -- brEw, W -- brOw, W -- brQw, W -- brXw,
        T -- brEw, T -- brOw, T -- brQw, T -- brXw,
        U -- brAw, U -- brEw, U -- brOw, U -- brQw, U -- brXw,
        V -- brAw, V -- brEw, V -- brOw, V -- brQw, V -- brXw,
        TR1 -- brAw, TR1 -- brEw, TR1 -- brOw, TR1 -- brQw, TR1 -- brXw,
        B -- brAe, B -- brEe, B -- brOe, B -- brQe, B -- brXe,
        C -- brAe, C -- brEe, C -- brOe, C -- brXe,
        D -- brAe, D -- brEe, D -- brOe,
        K -- brAe, K -- brOe, K -- brQe,
        L -- brAe, L -- brEe, L -- brOe, L -- brQe, L -- brXe,
        M -- brAe, M -- brEe, M -- brOe, M -- brQe, M -- brXe,
        N -- brAe, N -- brEe, N -- brOe, N -- brQe, N -- brXe,
        Q -- brAe, Q -- brQe, Q -- brXe,
        R -- brAe, R -- brEe, R -- brOe, R -- brQe, R -- brXe,
        S -- brAe, S -- brEe, S -- brOe, S -- brQe,
        X -- brAe, X -- brQe, X -- brXe,
        Y -- brAe, Y -- brEe, Y -- brOe, Y -- brQe, Y -- brXe,
        TR2 -- brAe, TR2 -- brEe, TR2 -- brOe, TR2 -- brQe, TR2 -- brXe,
        TR3 -- brAe, TR3 -- brEe, TR3 -- brOe, TR3 -- brQe, TR3 -- brXe,
        brXe -- brXw,
        brQe -- brQw,
        brAe -- brAw,
        brEe -- brEw,
        brOe -- brOw,
    );

    let graph = WeightedGraph {
        verts: VertexNames::verts(),
        edges,
    };

    let mut route = RouteGenerator::new(&graph, A.id(), [S.id(), N.id(), TR3.id(), G.id(), TR1.id(), Q.id(), J.id(), Y.id(), T.id(), O.id()]);
    // let mut last_route_step = Instant::now();
    let mut is_paused = false;
    let mut was_paused = false; // paused before giving command

    let mut camera = Camera3D::perspective(Vector3::new(0.0, 820.0, 0.0), Vector3::zero(), Vector3::new(0.0, 0.0, -1.0), 70.0);

    let mut is_giving_command = false;
    let mut command = String::new();
    let mut console_out: Vec<String> = Vec::with_capacity(5);

    while !rl.window_should_close() {
        if rl.is_key_pressed(KeyboardKey::KEY_ENTER) {
            is_giving_command = !is_giving_command;
            if is_giving_command {
                was_paused = is_paused;
                is_paused = true;
            } else {
                is_paused = was_paused;
            }
            if !is_giving_command { // finished giving command
                let mut targets = Vec::new();
                for id in command.split(' ') {
                    let candidates: Vec<VertexID> = graph.verts.iter()
                        .enumerate()
                        .filter(|(_, vert)| !vert.id.starts_with("Bridge"))
                        .filter(|(_, vert)| vert.id == id || vert.alias == id)
                        .map(|(v, _)| v as VertexID)
                        .collect();
                    match candidates.as_slice() {
                        &[] => {
                            console_out.push(format_args!("error: no vertex id {id}").to_string());
                        }
                        &[v] => {
                            if targets.contains(&v) {
                                console_out.push(format_args!("warning: vertex {v} is already a target").to_string());
                            } else {
                                console_out.push(format_args!("adding vertex {v} to targets").to_string());
                                targets.push(v);
                            }
                        }
                        &[..] => {
                            console_out.push(format_args!("error: {} matches for \"{id}\" including {:?}", candidates.len(), &candidates).to_string());
                        }
                    }
                }
                command.clear();
                let mut iter = targets.into_iter();
                if let Some(first) = iter.next() {
                    if iter.len() != 0 {
                        route = RouteGenerator::new(&graph, first, iter);
                    } else {
                        console_out.push(format_args!("warning: no targets provided").to_string());
                    }
                } else {
                    console_out.push(format_args!("warning: no start point provided").to_string());
                }
            }
        }

        if is_giving_command {
            if let Some(ch) = rl.get_char_pressed() {
                command.push(ch);
            } else if rl.is_key_pressed(KeyboardKey::KEY_BACKSPACE) {
                command.pop();
            }
        } else {
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

            if rl.is_key_pressed(KeyboardKey::KEY_SPACE) {
                is_paused = !is_paused;
            }
        }

        if !is_paused && !route.is_finished {
            // if last_route_step.elapsed() >= Duration::from_secs_f32(0.1) {
                // last_route_step = Instant::now();
                route.step(|msg| console_out.push(msg.to_string()));
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
                    if !route.is_finished && v == route.root {
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
            let text_width = d.measure_text(&vert.alias, 10);
            d.draw_text(&vert.alias, pos.x as i32 - text_width/2, pos.y as i32 - 5, 10, Color::WHITE);
            if let Some(Visit { distance, parent }) = route.visited[v as usize] {
                let parent_text = parent.map_or("-", |p| &graph.verts[p as usize].alias);
                let text = format!("{} ({parent_text})", distance.ceil());
                d.draw_text(&text, pos.x as i32 + text_width/2 + 3, pos.y as i32 + 3, 10, Color::GRAY);
            }
        }
        let route_text: Vec<&str> = route.result.iter().map(|&v| graph.verts[v as usize].id.as_str()).collect();
        let route_text = route_text.join(" - ");
        d.draw_text(&route_text, 0, 0, 10, Color::RAYWHITE);
        if is_giving_command {
            d.draw_text(&format!("> {command}"), 0, 10, 10, Color::LIGHTBLUE);
        } else {
            let target_text: Vec<&str> = route.targets.iter().map(|&v| graph.verts[v as usize].id.as_str()).collect();
            d.draw_text(&format!("> targets: {:?}", target_text), 0, 10, 10, Color::GREEN);
        }
        for (i, line) in console_out[console_out.len().saturating_sub(5)..].iter().enumerate() {
            d.draw_text(&format!("] {line}"), 0, 20 + 10*i as i32, 10, Color::DARKGRAY);
        }
    }
}
