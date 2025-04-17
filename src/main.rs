use std::collections::VecDeque;
use std::time::Instant;
use std::borrow::Cow;
use raylib::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConsoleLineCategory {
    Route,

    Command,
    TargetList,

    Trace,
    Debug,
    Info,
    Warning,
    Error,
    Fatal,
}

#[derive(Debug, Clone)]
pub struct ConsoleLine {
    cat: ConsoleLineCategory,
    msg: String,
}

impl ConsoleLine {
    pub fn as_line_ref(&self) -> ConsoleLineRef<'_> {
        ConsoleLineRef {
            cat: self.cat,
            msg: Cow::Borrowed(&self.msg),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConsoleLineRef<'a> {
    cat: ConsoleLineCategory,
    msg: Cow<'a, str>,
}

pub struct Console {
    pub route: ConsoleLine,
    pub input: ConsoleLine,
    pub route_debug: Vec<ConsoleLine>,
}

impl Console {
    pub const fn new() -> Self {
        Self {
            route: ConsoleLine {
                cat: ConsoleLineCategory::Route,
                msg: String::new(),
            },
            input: ConsoleLine {
                cat: ConsoleLineCategory::TargetList,
                msg: String::new(),
            },
            route_debug: Vec::new(),
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = ConsoleLineRef<'_>> {
        std::iter::once(self.input.as_line_ref())
        .chain(self.route_debug.iter().map(|item| item.as_line_ref()))
    }

    pub fn write(&mut self, cat: ConsoleLineCategory, depth: usize, msg: std::fmt::Arguments<'_>) {
        while self.route_debug.len() > depth {
            self.route_debug.pop();
        }
        if self.route_debug.len() == depth {
            self.route_debug.push(ConsoleLine { cat, msg: msg.to_string() });
        } else {
            panic!("cannot push more than one depth (current depth: {}, write depth: {})\nconsole: {:?}\nwanted to print: \"{:?}\"",
                self.route_debug.len() as isize - 1,
                depth,
                &self.route_debug,
                msg,
            );
        }
    }
}

macro_rules! write_cout {
    ($dst:expr, $level:ident, $depth:expr, $($args:tt)+) => {
        $dst.write(
            ConsoleLineCategory::$level,
            $depth,
            format_args!($($args)+),
        )
    };
}

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

#[derive(Debug, Clone)]
pub struct WeightedGraph {
    pub verts: Vec<Vertex>,
    pub edges: Vec<Edge>,
    pub adjacent: Vec<Vec<Adjacent>>,
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

    pub fn verts_iter(&self) -> impl ExactSizeIterator<Item = (VertexID, &Vertex)> + DoubleEndedIterator {
        self.verts.iter().enumerate().map(|(v, vert)| (v as VertexID, vert))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Visit {
    distance: f32,
    parent: Option<VertexID>,
}
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
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
            is_finished: false,
        }
    }

    fn begin_phase_none(&mut self, _console: &mut Console) {
        todo!()
    }

    fn begin_phase_edge(&mut self, console: &mut Console) {
        write_cout!(console, Info, 1, "edge phase");
        write_cout!(console, Debug, 2, "looking at vertex {}", self.root);
        self.queue.clear();
        self.visited.fill(const { None });
        self.visited[self.root as usize] = Some(Visit { distance: 0.0, parent: None });
        self.phase = Phase::Edge {
            current: self.root,
            i: 0,
        };
    }

    fn begin_phase_target(&mut self, console: &mut Console) {
        write_cout!(console, Info, 1, "target phase");
        self.phase = Phase::Target {
            shortest_distance: f32::INFINITY,
            nearest_target: usize::MAX,
            i: 0,
        };
    }

    fn begin_phase_backtrack(&mut self, console: &mut Console) {
        write_cout!(console, Info, 1, "backtrack phase");
        let insert_at = self.result.len();
        self.phase = Phase::Backtrack {
            parent: self.root,
            insert_at,
            i: 0,
        };
    }

    pub fn step(&mut self, console: &mut Console) {
        debug_assert!(!self.is_finished, "do not continue finished route");
        match &mut self.phase {
            Phase::None => {
                write_cout!(console, Info, 0, "root set to vertex {}; targeting {:?}", self.root, self.targets);
                self.begin_phase_edge(console);
            }

            Phase::Edge { current, i } => {
                if let Some(&Adjacent { vertex, weight }) = self.graph.adjacent[*current as usize].get(*i) {
                    if self.visited[vertex as usize].is_none() {
                        self.queue.push_back(vertex);
                        write_cout!(console, Info, 3, "pushed vertex {vertex} to queue");
                        for &Adjacent { vertex, .. } in &self.graph.adjacent[vertex as usize] {
                            if !self.queue.contains(&vertex) {
                                self.queue.push_back(vertex);
                                write_cout!(console, Info, 3, "pushed vertex {vertex} to queue");
                            }
                        }
                    }
                    let distance = self.visited[*current as usize].expect("current must have been visited if it is queued").distance + weight;
                    if self.visited[vertex as usize].is_none_or(|visit| distance < visit.distance) {
                        self.visited[vertex as usize] = Some(Visit { distance, parent: Some(*current) });
                        write_cout!(console, Info, 3, "vertex {vertex} is {distance} from root (vertex {}) through vertex {current}, new best", self.root);
                    }
                    *i += 1;
                } else {
                    if let Some(next_vert) = self.queue.pop_front() {
                        *current = next_vert;
                        *i = 0;
                        write_cout!(console, Debug, 2, "looking at vertex {current}");
                    } else {
                        self.begin_phase_target(console);
                    }
                }
            }

            Phase::Target { shortest_distance, nearest_target, i } => {
                if *i < self.targets.len() {
                    let v = self.targets[*i];
                    if let Some(Visit { distance, .. }) = self.visited[v as usize] {
                        write_cout!(console, Info, 2, "target {} (vertex {v}) is {distance} from root (vertex {})", *i, self.root);
                        if distance < *shortest_distance {
                            write_cout!(console, Info, 3, "updating nearest target to {} (vertex {v})", *i);
                            *shortest_distance = distance;
                            *nearest_target = *i;
                        }
                        *i += 1;
                    }
                } else {
                    assert!(*nearest_target < self.targets.len());
                    self.root = self.targets.swap_remove(*nearest_target);
                    write_cout!(console, Info, 2, "nearest target identified as vertex {}", self.root);
                    self.begin_phase_backtrack(console);
                }
            }

            Phase::Backtrack { parent, insert_at, i } => {
                if let Some(Visit { parent: Some(p), .. }) = &self.visited[*parent as usize] {
                    self.result.insert(*insert_at, *parent);
                    *parent = *p;
                } else {
                    write_cout!(console, Info, 2, "adding vertex {} to results", self.root);
                    if self.targets.is_empty() {
                        write_cout!(console, Info, 0, "final route: {:?}", self.result);
                        self.is_finished = true;
                        let mut distance = 0.0;
                        self.visited[self.result[0] as usize] = Some(Visit { distance, parent: None });
                        if *i + 1 < self.result.len() {
                            let a = &self.result[*i];
                            let b = &self.result[*i + 1];
                            distance += self.graph.adjacent[*a as usize].iter()
                                .find_map(|e| (&e.vertex == b).then_some(e.weight))
                                .expect("results should be adjacent");
                            self.visited[*b as usize] = Some(Visit { distance, parent: Some(*a) });
                            *i += 1;
                        }
                    } else {
                        self.begin_phase_edge(console);
                    }
                }
            }
        }
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
            pub const fn id(self) -> VertexID {
                self as VertexID
            }
        }
        #[allow(unused)]
        use VertexNames::{$($v_id as $v_alias),*};
        let $name = vec![$(Vertex::new(stringify!($v_id), stringify!($v_alias), $x, $y, $z)),*];
    };
}

macro_rules! define_edges {
    ($name:ident: $($a:ident--$b:ident),* $(,)?) => {
        let $name = vec![$(
            Edge {
                id: None,
                adj: [$a.id(), $b.id()],
                weight: $a.distance_to($b),
            },
        )*];
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

    rl.set_target_fps(120);

    define_verts!{
        verts:
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

    define_edges!{
        edges:
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
    }

    let graph = WeightedGraph::new(verts, edges);

    let mut route = RouteGenerator::new(&graph, A.id(), [S.id(), N.id(), TR3.id(), G.id(), TR1.id(), Q.id(), J.id(), Y.id(), T.id(), O.id()]);
    let mut is_paused = false;
    let mut was_paused = false; // paused before giving command

    let mut camera = Camera3D::perspective(Vector3::new(0.0, 820.0, 0.0), Vector3::zero(), Vector3::new(0.0, 0.0, -1.0), 70.0);

    let mut is_giving_command = false;
    let mut console: Console = Console::new();

    let mut last_route_step = Instant::now();
    let mut tempo_ticks = 1;
    let mut tempo_ms = 16;

    while !rl.window_should_close() {
        if rl.is_key_pressed(KeyboardKey::KEY_ENTER) {
            is_giving_command = !is_giving_command;
            if is_giving_command {
                // begin giving command
                was_paused = is_paused;
                is_paused = true;

                console.input.cat = ConsoleLineCategory::Command;
                console.input.msg = String::new();
            } else {
                // finish giving command
                is_paused = was_paused;

                let mut targets = Vec::new();
                let command = std::mem::replace(&mut console.input.msg, String::new());
                for id in command.split(' ') {
                    let candidates: Vec<VertexID> = graph.verts_iter()
                        .filter_map(|(v, vert)| (!vert.id.starts_with("Bridge") && (vert.id == id || vert.alias == id)).then_some(v))
                        .collect();
                    match candidates.as_slice() {
                        &[] => {
                            write_cout!(console, Error, 0, "no vertex id {id}");
                        }
                        &[v] => {
                            if targets.contains(&v) {
                                write_cout!(console, Error, 0, "vertex {v} is already a target");
                            } else {
                                write_cout!(console, Info, 0, "adding vertex {v} to targets");
                                targets.push(v);
                            }
                        }
                        &[..] => {
                            write_cout!(console, Error, 0, "{} matches for \"{id}\" including {:?}", candidates.len(), &candidates);
                        }
                    }
                }
                let mut iter = targets.into_iter();
                if let Some(first) = iter.next() {
                    if iter.len() != 0 {
                        route = RouteGenerator::new(&graph, first, iter);
                    } else {
                        write_cout!(console, Warning, 0, "no targets provided");
                    }
                } else {
                    write_cout!(console, Warning, 0, "no start point provided");
                }
            }
        }

        if is_giving_command {
            if let Some(ch) = rl.get_char_pressed() {
                console.input.msg.push(ch);
            } else if rl.is_key_pressed(KeyboardKey::KEY_BACKSPACE) {
                console.input.msg.pop();
            }
        } else {
            let speed = 4.0*camera.position.distance_to(camera.target)/1000.0;
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
                if !is_paused && rl.is_key_down(KeyboardKey::KEY_LEFT_SHIFT) { // sprint
                    while !route.is_finished {
                        route.step(&mut console);
                    }
                } else {
                    is_paused = !is_paused;
                }
            }
        }

        if !is_paused && !route.is_finished {
            if let Some(tempo_ms) = std::num::NonZeroU128::new(tempo_ms) {
                let ticks = last_route_step.elapsed().as_millis()/tempo_ms;
                if ticks > 0 {
                    for _ in 0..ticks*tempo_ticks {
                        route.step(&mut console);
                    }
                    last_route_step = Instant::now();
                }
            } else {
                for _ in 0..tempo_ticks {
                    route.step(&mut console);
                }
            }
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

            for (v, vert) in graph.verts_iter() {
                let distance_from_target = vert.pos - camera.target;
                let color = loop {
                    if let Phase::Edge { current, i } = &route.phase {
                        if &v == current {
                            break Color::SKYBLUE;
                        } else if graph.adjacent[*current as usize].get(*i).is_some_and(|x| &v == &x.vertex) {
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

        for (v, vert) in graph.verts_iter() {
            let pos = d.get_world_to_screen(vert.pos, camera);
            let text_width = d.measure_text(&vert.alias, 10);
            d.draw_text(&vert.alias, pos.x as i32 - text_width/2, pos.y as i32 - 5, 10, Color::WHITE);
            if let Some(Visit { distance, parent }) = route.visited[v as usize] {
                let parent_text = parent.map_or("-", |p| &graph.verts[p as usize].alias);
                let text = format!("{} ({parent_text})", distance.ceil());
                d.draw_text(&text, pos.x as i32 + text_width/2 + 3, pos.y as i32 + 3, 10, Color::GRAY);
            }
        }

        // Console

        console.route.msg = route.result.iter()
            .map(|&v| graph.verts[v as usize].id.as_str())
            .collect::<Vec<&str>>()
            .join(" - ");

        if !is_giving_command {
            let target_text = route.targets.iter()
                .map(|&v| graph.verts[v as usize].id.as_str())
                .collect::<Vec<&str>>()
                .join(", ");
            console.input = ConsoleLine {
                cat: ConsoleLineCategory::TargetList,
                msg: target_text,
            }
        }

        for (i, line) in console.iter().enumerate() {
            let (color, prefix) = match line.cat {
                ConsoleLineCategory::Route      => (Color::RAYWHITE,  "route: "),
                ConsoleLineCategory::Command    => (Color::LIGHTBLUE, "> "),
                ConsoleLineCategory::TargetList => (Color::LIME,      "targets: "),
                ConsoleLineCategory::Trace      => (Color::DARKGRAY,  "trace: "),
                ConsoleLineCategory::Debug      => (Color::MAGENTA,   "debug: "),
                ConsoleLineCategory::Info       => (Color::LIGHTGRAY, ""),
                ConsoleLineCategory::Warning    => (Color::GOLD,      "warning: "),
                ConsoleLineCategory::Error      => (Color::RED,       "error: "),
                ConsoleLineCategory::Fatal      => (Color::SALMON,    "fatal: "),
            };
            d.draw_text(&format!("{prefix}{}", line.msg), 0, 10*i as i32, 10, color);
        }
    }
}
