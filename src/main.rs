use std::collections::VecDeque;
use std::time::{Duration, Instant};
use std::borrow::Cow;
use paste::paste;
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
        ConsoleLineRef::new(self.cat, &self.msg)
    }
}

#[derive(Debug, Clone)]
pub struct ConsoleLineRef<'a> {
    cat: ConsoleLineCategory,
    msg: Cow<'a, str>,
}

impl<'a> ConsoleLineRef<'a> {
    pub fn new(cat: ConsoleLineCategory, msg: &'a str) -> Self {
        Self {
            cat,
            msg: Cow::Borrowed(msg),
        }
    }

    pub fn route(msg: &'a str) -> Self {
        Self::new(ConsoleLineCategory::Route, msg)
    }
    pub fn command(msg: &'a str) -> Self {
        Self::new(ConsoleLineCategory::Command, msg)
    }
    pub fn target_list(msg: &'a str) -> Self {
        Self::new(ConsoleLineCategory::TargetList, msg)
    }
}

pub struct Console {
    pub route: String,
    pub command: String,
    pub reply: Vec<ConsoleLine>,
    pub target: Option<String>,
    pub debug: Vec<ConsoleLine>,
}

impl Console {
    pub const fn new() -> Self {
        Self {
            route: String::new(),
            command: String::new(),
            reply: Vec::new(),
            target: None,
            debug: Vec::new(),
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = ConsoleLineRef<'_>> {
        None.into_iter()
            .chain(std::iter::once_with(|| ConsoleLineRef::command(&self.command)))
            .chain(self.reply.iter().map(|item| item.as_line_ref()))
            // .chain(std::iter::once(ConsoleLineRef::route(&self.route)))
            // .chain(self.target.iter().map(|msg| ConsoleLineRef::target_list(msg)))
            .chain(self.debug.iter().map(|item| item.as_line_ref()))
    }

    pub fn reply(&mut self, cat: ConsoleLineCategory, msg: std::fmt::Arguments<'_>) {
        self.reply.push(ConsoleLine { cat, msg: msg.to_string() });
    }

    pub fn debug(&mut self, cat: ConsoleLineCategory, depth: usize, msg: std::fmt::Arguments<'_>) {
        while self.debug.len() > depth {
            self.debug.pop();
        }
        if self.debug.len() == depth {
            self.debug.push(ConsoleLine { cat, msg: msg.to_string() });
        } else {
            panic!("cannot push more than one depth (current depth: {}, write depth: {})\nconsole: {:?}\nwanted to print: \"{:?}\"",
                self.debug.len() as isize - 1,
                depth,
                &self.debug,
                msg,
            );
        }
    }
}

macro_rules! write_cout {
    (@reply: $cons:expr, $level:ident, $($args:tt)+) => {
        $cons.reply(
            ConsoleLineCategory::$level,
            format_args!($($args)+),
        )
    };

    ($cons:expr, $level:ident, $depth:expr, $($args:tt)+) => {
        $cons.debug(
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
    pub graph: &'a WeightedGraph,
    pub targets: Vec<VertexID>,
    pub result: Vec<VertexID>,
    pub root: VertexID,
    pub visited: Box<[Option<Visit>]>,
    pub queue: VecDeque<VertexID>,
    pub phase: Phase,
    pub is_finished: bool,
}

impl<'a> RouteGenerator<'a> {
    pub fn new(graph: &'a WeightedGraph, start: VertexID, targets: impl IntoIterator<Item = VertexID>) -> Self {
        Self {
            graph,
            targets: Vec::from_iter(targets),
            result: vec![start],
            root: start,
            visited: vec![const { None }; graph.verts.len()].into_boxed_slice(),
            queue: VecDeque::new(),
            phase: Phase::None,
            is_finished: false,
        }
    }

    fn begin_phase_edge(&mut self, console: &mut Console) {
        write_cout!(console, Info, 1, "edge phase");
        write_cout!(console, Info, 2, "looking at vertex {}", self.root);
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

    pub fn add_targets(&mut self, console: &mut Console, targets: impl IntoIterator<Item = VertexID>) {
        self.is_finished = false;
        self.targets.extend(targets);
        self.begin_phase_edge(console);
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
                    write_cout!(console, Info, 2, "looking at vertex {current} ({i}/{})", self.graph.adjacent[*current as usize].len());
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
                    write_cout!(console, Info, 3, "vertex {vertex} is {distance} from root (vertex {}) through vertex {current}", self.root);
                    if self.visited[vertex as usize].is_none_or(|visit| distance < visit.distance) {
                        self.visited[vertex as usize] = Some(Visit { distance, parent: Some(*current) });
                        write_cout!(console, Info, 4, "new best");
                    }
                    *i += 1;
                } else {
                    if let Some(next_vert) = self.queue.pop_front() {
                        *current = next_vert;
                        *i = 0;
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
                    } else {
                        write_cout!(console, Error, 0, "no route exists");
                        self.is_finished = true;
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
                        let final_route = self.result.iter()
                            .map(|&v| self.graph.verts[v as usize].id.as_str())
                            .collect::<Vec<&str>>()
                            .join(" - ");
                        console.reply.clear();
                        write_cout!(console, Route, 0, "{final_route}");
                        self.is_finished = true;
                        let mut distance = 0.0;
                        self.visited.fill(const { None });
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
                        write_cout!(console, Route, 1, "total distance: {distance}");
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

pub enum ParseCoordsError {
    Syntax,
    ParseFloat(std::num::ParseFloatError),
}
impl std::fmt::Display for ParseCoordsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseCoordsError::Syntax => write!(f, "expected x:<???>/y:<???>"),
            ParseCoordsError::ParseFloat(e) => write!(f, "{e}"),
        }
    }
}

pub fn parse_coords(coords: &str) -> Result<Vector3, ParseCoordsError> {
    let (x, y) = coords.split_once('/').ok_or(ParseCoordsError::Syntax)?;
    let x = if x.starts_with("x:") { x[2..].parse().map_err(|e| ParseCoordsError::ParseFloat(e))? } else { return Err(ParseCoordsError::Syntax) };
    let y = if y.starts_with("y:") { y[2..].parse().map_err(|e| ParseCoordsError::ParseFloat(e))? } else { return Err(ParseCoordsError::Syntax) };
    Ok(Vector3::new(x, 0.0, y))
}

// pub struct ArgMeta {
//     pub prefix: &'static str,
//     pub alternates: &'static [&'static str],
//     pub is_literal: bool,
//     pub is_optional: bool,
//     pub is_repeating: bool,
//     pub separator: &'static str,
// }
// pub struct UsageMeta {
//     pub args: &'static [ArgMeta],
//     pub blurb: &'static str,
// }
// impl UsageMeta {
//     pub fn args_to_string(&self) -> String {
//         let mut req = 0;
//         for arg in self.args {
//             if arg.is_literal {

//             }
//         }
//         let result = String::new();
//     }
// }
// pub struct CommandMeta {
//     pub id: Cmd,
//     pub path: &'static [&'static str],
//     pub usage: &'static [UsageMeta],
// }
// impl CommandMeta {
//     pub fn usage_msg(list: &[&Self]) -> String {
//         let args = list.iter().map(|item| item.usage.iter().map(|usage| usage.args.));
//         let max_len = list.iter()
//             .max_by(|a, b| a.cmp(b));
//         let items = list.map(format!())
//         format!("usage:{}")
//     }
// }
// macro_rules! define_commands {
//     ($(> $($path:ident)/+ $(: $({$($args:tt)+})* #[$blurb:literal])+)+) => {
//         paste!{
//             #[allow(non_camel_case_types)]
//             pub enum Cmd {
//                 $([< $($path)_+ >]),+
//             }
//             impl CommandMeta {
//                 $(
//                     #[allow(non_upper_case_globals)]
//                     pub const [< $($path)_+ >]: Self = Self {
//                         id: Cmd::[< $($path)_+ >],
//                         path: &[$(stringify!($path)),+],
//                         usage: &[$(
//                             UsageMeta {
//                                 args: &[$(define_commands!(@args: $($args)+)),*],
//                                 blurb: $blurb,
//                             }
//                         ),+]
//                     };
//                 )+

//                 pub const LIST: &[Self] = &[$(Self::[< $($path)_+ >]),+];

//                 pub fn try_from_str(path: &str) -> Result<&'static Self, &str> {
//                     match path {
//                         $(stringify!($($path).+) => Ok(&Self::[< $($path)_+ >]),)+
//                         _ => Err(path),
//                     }
//                 }
//             }
//         }
//     };

//     (@args: $($prefix:literal-)? #($($alternates:literal)or+)?.. $($separator:literal)?) => {
//         ArgMeta {
//             prefix: define_commands!(@prefix: $($prefix)?),
//             alternates: &[$($alternates),+],
//             is_literal: true,
//             is_optional: true,
//             is_repeating: true,
//             separator: define_commands!(@separator: $($separator)?),
//         }
//     };
//     (@args: $($prefix:literal-)? ($($alternates:literal)or+)?.. $($separator:literal)?) => {
//         ArgMeta {
//             prefix: define_commands!(@prefix: $($prefix)?),
//             alternates: &[$($alternates),+],
//             is_literal: false,
//             is_optional: true,
//             is_repeating: true,
//             separator: define_commands!(@separator: $($separator)?),
//         }
//     };
//     (@args: $($prefix:literal-)? #($($alternates:literal)or+).. $($separator:literal)?) => {
//         ArgMeta {
//             prefix: define_commands!(@prefix: $($prefix)?),
//             alternates: &[$($alternates),+],
//             is_literal: true,
//             is_optional: false,
//             is_repeating: true,
//             separator: define_commands!(@separator: $($separator)?),
//         }
//     };
//     (@args: $($prefix:literal-)? ($($alternates:literal)or+).. $($separator:literal)?) => {
//         ArgMeta {
//             prefix: define_commands!(@prefix: $($prefix)?),
//             alternates: &[$($alternates),+],
//             is_literal: false,
//             is_optional: false,
//             is_repeating: true,
//             separator: define_commands!(@separator: $($separator)?),
//         }
//     };
//     (@args: $($prefix:literal-)? #($($alternates:literal)or+)? $($separator:literal)?) => {
//         ArgMeta {
//             prefix: define_commands!(@prefix: $($prefix)?),
//             alternates: &[$($alternates),+],
//             is_literal: true,
//             is_optional: true,
//             is_repeating: false,
//             separator: define_commands!(@separator: $($separator)?),
//         }
//     };
//     (@args: $($prefix:literal-)? ($($alternates:literal)or+)? $($separator:literal)?) => {
//         ArgMeta {
//             prefix: define_commands!(@prefix: $($prefix)?),
//             alternates: &[$($alternates),+],
//             is_literal: false,
//             is_optional: true,
//             is_repeating: false,
//             separator: define_commands!(@separator: $($separator)?),
//         }
//     };
//     (@args: $($prefix:literal-)? #($($alternates:literal)or+) $($separator:literal)?) => {
//         ArgMeta {
//             prefix: define_commands!(@prefix: $($prefix)?),
//             alternates: &[$($alternates),+],
//             is_literal: true,
//             is_optional: false,
//             is_repeating: false,
//             separator: define_commands!(@separator: $($separator)?),
//         }
//     };
//     (@args: $($prefix:literal-)? ($($alternates:literal)or+) $($separator:literal)?) => {
//         ArgMeta {
//             prefix: define_commands!(@prefix: $($prefix)?),
//             alternates: &[$($alternates),+],
//             is_literal: false,
//             is_optional: false,
//             is_repeating: false,
//             separator: define_commands!(@separator: $($separator)?),
//         }
//     };

//     (@prefix: $prefix:literal) => { $prefix };
//     (@prefix: ) => { "" };

//     (@separator: $separator:literal) => { $separator };
//     (@separator: ) => { " " };
// }

// define_commands!{
//     > help
//         :                                                                   #["display this information"]

//     > dbg
//         :                                                                   #["toggle debug messages"]

//     > cam/focus
//         : { ("ID" or "ALIAS") }                                             #["zoom in on a particular target"]
//         : { #("reset") }                                                    #["reset camera orientation"]
//         :                                                                   #["print the name of the focused vertex"]

//     > sv/route
//         : { ("START") } { ("ID" or "ALIAS").. }                             #["generate the shortest route visiting each target (separated by spaces)"]

//     > sv/route/add
//         : { ("ID" or "ALIAS").. }                                           #["add more targets (separated by spaces) to the current route"]

//     > sv/new
//         : { ("ID") } { ("ALIAS")? } { "x:"- ("???") "/" } { "y:"- ("???") } #["create a new vertex that can be targeted at x,y"]
//         : { ("ID") } { ("ALIAS")? } { #("focus") }                          #["create a new vertex that can be targeted at the focused position"]

//     > sv/edge
//         : { ("ID" or "ALIAS") } { ("ID" or "ALIAS") }                       #["create an edge connecting two existing vertices"]
//         : { ("ID" or "ALIAS") }                                             #["print a list of all vertices adjacent to the target"]

//     > tempo
//         : { ("TICKS") "/" } { ("MILLISECONDS") }                            #["set the route tick speed in ticks per milliseconds"]
//         : { #("reset") }                                                    #["set the route tick speed to the default (1 step per frame)"]
//         :                                                                   #["print the current tempo"]
// }

fn main() {
    const VERTEX_RADIUS: f32 = 8.0;

    define_verts!{
        verts:
        // Satellites
        Alpha    (A) = (     0.0, 0.0,      0.0);
        Bravo    (B) = (-  100.0, 0.0, -  200.0);
        Charlie  (C) = (     0.0, 0.0, -  200.0);
        Delta    (D) = (   100.0, 0.0, -  200.0);
        Echo     (E) = (   200.0, 0.0, -  100.0);
        Foxtrot  (F) = (   200.0, 0.0,      0.0);
        Golf     (G) = (   200.0, 0.0,    100.0);
        Hotel    (H) = (   100.0, 0.0,    200.0);
        India    (I) = (     0.0, 0.0,    200.0);
        Juliett  (J) = (-  100.0, 0.0,    200.0);
        Kilo     (K) = (-  200.0, 0.0,    100.0);
        Lima     (L) = (-  200.0, 0.0,      0.0);
        Mike     (M) = (-  200.0, 0.0, -  100.0);
        November (N) = (-  300.0, 0.0, -  300.0);
        Oscar    (O) = (   300.0, 0.0, -  300.0);
        Papa     (P) = (   300.0, 0.0,    300.0);
        Quebec   (Q) = (-  300.0, 0.0,    300.0);
        Romeo    (R) = (-  500.0, 0.0, -  500.0);
        Sierra   (S) = (     0.0, 0.0, -  500.0);
        Tango    (T) = (   500.0, 0.0, -  500.0);
        Uniform  (U) = (   500.0, 0.0,      0.0);
        Victor   (V) = (   500.0, 0.0,    500.0);
        Whiskey  (W) = (     0.0, 0.0,    500.0);
        Xray     (X) = (-  500.0, 0.0,    500.0);
        Yankee   (Y) = (-  500.0, 0.0,      0.0);
        Zulu     (Z) = ( 10000.0, 0.0,  10000.0);

        // Transformers
        TR_1 (TR1) = ( 400.0, 0.0,  200.0); // Transformer 1
        TR_2 (TR2) = (-540.0, 0.0,  232.0); // Transformer 2
        TR_3 (TR3) = (-400.0, 0.0, -475.0); // Transformer 3

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

    let mut graph = WeightedGraph::new(verts, edges);

    let mut route = None;

    const CAMERA_POSITION_DEFAULT: Vector3 = Vector3::new(0.0, 1300.0, 0.0);
    let mut camera = Camera3D::perspective(
        CAMERA_POSITION_DEFAULT,
        Vector3::zero(),
        Vector3::new(0.0, 0.0, -1.0),
        45.0,
    );

    let mut is_giving_command = false;
    let mut command_history = VecDeque::new();
    let mut command_history_offset = 0;
    let mut console: Console = Console::new();
    let mut is_debugging = false;

    let mut tempo_ticks = 1;
    let mut tempo_ms = 16;

    let mut is_cursor_shown = false;
    let mut cursor_last_toggled = Instant::now();

    let mut backspace_pressed = None;

    let (mut rl, thread) = init()
        .title("VotV Route Tool")
        .resizable()
        .msaa_4x()
        .build();

    rl.hide_cursor();
    rl.maximize_window();
    rl.set_target_fps(120);
    rl.set_exit_key(None);

    // let img = Image::load_image_from_mem(".png", include_bytes!("resources/console.png")).unwrap();
    // let font = dbg!(rl.load_font_from_image(&thread, &img, Color::MAGENTA, ' ' as i32).unwrap());
    let font = rl.load_font_from_memory(&thread, ".ttf", include_bytes!("resources/ShareTechMono-Regular.ttf"), 16, None).unwrap();

    'window: while !rl.window_should_close() {
        if rl.is_key_pressed(KeyboardKey::KEY_ESCAPE) {
            is_giving_command = false;
            console.command.clear();
        }
        if rl.is_key_pressed(KeyboardKey::KEY_ENTER) {
            is_giving_command = !is_giving_command;
            if is_giving_command {
                // begin giving command
                is_cursor_shown = true;
                cursor_last_toggled = Instant::now();

                console.command.clear();
                console.reply.clear();
            } else {
                // finish giving command
                if !console.command.is_empty() {
                    command_history.push_front(console.command.clone());
                    command_history_offset = 0;let mut args = command_history.front().unwrap().split(' ');

                    if let Some(cmd) = args.next() {
                        #[allow(non_camel_case_types)]
                        enum Cmd {
                            CMD_HELP,
                            CMD_CLOSE,
                            CMD_DBG,
                            CMD_FOCUS,
                            CMD_SV_ROUTE,
                            CMD_SV_ROUTE_ADD,
                            CMD_SV_NEW,
                            CMD_SV_EDGE,
                            CMD_TEMPO,
                        }
                        use Cmd::*;
                        impl Cmd {
                            const LIST: [Self; 9] = [
                                CMD_HELP,
                                CMD_CLOSE,
                                CMD_DBG,
                                CMD_FOCUS,
                                CMD_SV_ROUTE,
                                CMD_SV_ROUTE_ADD,
                                CMD_SV_NEW,
                                CMD_SV_EDGE,
                                CMD_TEMPO,
                            ];
                            fn try_from_str(s: &str) -> Result<Self, &str> {
                                match s {
                                    "help" => Ok(CMD_HELP),
                                    "close" => Ok(CMD_CLOSE),
                                    "dbg" => Ok(CMD_DBG),
                                    "focus" => Ok(CMD_FOCUS),
                                    "sv.route" => Ok(CMD_SV_ROUTE),
                                    "sv.route.add" => Ok(CMD_SV_ROUTE_ADD),
                                    "sv.new" => Ok(CMD_SV_NEW),
                                    "sv.edge" => Ok(CMD_SV_EDGE),
                                    "tempo" => Ok(CMD_TEMPO),
                                    _ => Err(s),
                                }
                            }
                            const fn as_str(&self) -> &'static str {
                                match self {
                                    CMD_HELP => "help",
                                    CMD_CLOSE => "close",
                                    CMD_DBG => "dbg",
                                    CMD_FOCUS => "focus",
                                    CMD_SV_ROUTE => "sv.route",
                                    CMD_SV_ROUTE_ADD => "sv.route.add",
                                    CMD_SV_NEW => "sv.new",
                                    CMD_SV_EDGE => "sv.edge",
                                    CMD_TEMPO => "tempo",
                                }
                            }
                            fn usage(&self) -> Vec<String> {
                                match self {
                                    CMD_HELP => vec![
                                        format!("{self}"),
                                    ],
                                    CMD_CLOSE => vec![
                                        format!("{self}"),
                                    ],
                                    CMD_DBG => vec![
                                        format!("{self}"),
                                    ],
                                    CMD_FOCUS => vec![
                                        format!("{self} <ID|ALIAS>"),
                                        format!("{self} reset"),
                                        format!("{self}"),
                                    ],
                                    CMD_SV_ROUTE => vec![
                                        format!("{self} <START> <ID|ALIAS>..."),
                                    ],
                                    CMD_SV_ROUTE_ADD => vec![
                                        format!("{self} <ID|ALIAS>..."),
                                    ],
                                    CMD_SV_NEW => vec![
                                        format!("{self} <ID> [ALIAS] x:<???>/y:<???>"),
                                        format!("{self} <ID> [ALIAS] focus"),
                                    ],
                                    CMD_SV_EDGE => vec![
                                        format!("{self} <ID|ALIAS> <ID|ALIAS>"),
                                        format!("{self} <ID|ALIAS>"),
                                    ],
                                    CMD_TEMPO => vec![
                                        format!("{self} <TICKS>/<MILLISECONDS>"),
                                        format!("{self} reset"),
                                        format!("{self} sprint"),
                                        format!("{self}"),
                                    ],
                                }
                            }
                            fn description(&self) -> &'static [&'static str] {
                                match self {
                                    CMD_HELP => &[
                                        "display this information",
                                    ],
                                    CMD_CLOSE => &[
                                        "close the application",
                                    ],
                                    CMD_DBG => &[
                                        "toggle debug messages",
                                    ],
                                    CMD_FOCUS => &[
                                        "zoom in on a particular target",
                                        "reset camera orientation",
                                        "print the name of the focused vertex",
                                    ],
                                    CMD_SV_ROUTE => &[
                                        "generate the shortest route visiting each target (separated by spaces)",
                                    ],
                                    CMD_SV_ROUTE_ADD => &[
                                        "add more targets (separated by spaces) to the current route",
                                    ],
                                    CMD_SV_NEW => &[
                                        "create a new vertex that can be targeted at x,y",
                                        "create a new vertex that can be targeted at the focused position",
                                    ],
                                    CMD_SV_EDGE => &[
                                        "create an edge connecting two existing vertices",
                                        "print a list of all vertices adjacent to the target",
                                    ],
                                    CMD_TEMPO => &[
                                        "set the route tick speed in ticks per milliseconds",
                                        "set the route tick speed to the default (1 step per frame)",
                                        "set the route tick speed to the maximum (1 step every 0 milliseconds)",
                                        "print the current tempo",
                                    ],
                                }
                            }
                        }
                        impl std::fmt::Display for Cmd {
                            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                                write!(f, "{}", self.as_str())
                            }
                        }
                        match Cmd::try_from_str(cmd) {
                            Ok(cmd) => {
                                let mut check_usage = false;
                                match cmd {
                                    CMD_HELP => {
                                        write_cout!(@reply: console, Info, "commands:{}",
                                            Cmd::LIST.iter()
                                                .flat_map(|item| item.usage().into_iter().zip(item.description()))
                                                .map(|(usage, desc)| format!("\n    {usage:<35}  {desc}"))
                                                .collect::<Vec<_>>()
                                                .concat(),
                                        );
                                    }

                                    CMD_SV_ROUTE => {
                                        let mut target_iter = args.map(|id| graph.find_vert(id));

                                        if let Some(first) = target_iter.next() {
                                            match first {
                                                Ok(start) => {
                                                    let mut target_iter = target_iter.filter_map(|v| {
                                                        v.inspect(|v| write_cout!(@reply: console, Info, "adding vertex {v} to targets"))
                                                        .inspect_err(|id| write_cout!(@reply: console, Warning, "vertex \"{id}\" does not exist, skipping"))
                                                        .ok()
                                                    }).peekable();
                                                    if target_iter.peek().is_some() {
                                                        route = Some((RouteGenerator::new(&graph, start, target_iter), Instant::now()));
                                                        write_cout!(@reply: console, Info, "generating route");
                                                    } else {
                                                        write_cout!(@reply: console, Error, "no targets provided");
                                                    }
                                                }
                                                Err(start) => {
                                                    write_cout!(@reply: console, Error, "vertex \"{start}\" does not exist");
                                                }
                                            }
                                        } else { check_usage = true; }
                                    }

                                    CMD_SV_ROUTE_ADD => {
                                        if let Some((route, _)) = &mut route {
                                            let target_iter = args.filter_map(|id| graph.find_vert(id).ok());
                                            route.add_targets(&mut console, target_iter);
                                            write_cout!(@reply: console, Info, "extending route");
                                        } else {
                                            write_cout!(@reply: console, Error, "no ongoing route to extend");
                                        }
                                    }

                                    CMD_SV_NEW => {
                                        if let Some(id) = args.next() {
                                            let mut alias = args.next();
                                            let coords = args.next().or_else(|| alias.take());
                                            if let Some(coords) = coords {
                                                let pos = if coords == "focus" {
                                                    Some(camera.target)
                                                } else {
                                                    match parse_coords(coords) {
                                                        Ok(pos) => Some(pos),
                                                        Err(e) => {
                                                            write_cout!(@reply: console, Error, "could not parse coordinates: {e}");
                                                            None
                                                        },
                                                    }
                                                };
                                                if let Some(pos) = pos {
                                                    graph.add_vertex(id, alias.unwrap_or(id), pos);
                                                    route = None;
                                                    write_cout!(@reply: console, Info, "created new vertex");
                                                }
                                            } else { check_usage = true; }
                                        } else { check_usage = true; }
                                    }

                                    CMD_SV_EDGE => {
                                        if let Some(a) = args.next() {
                                            if let Some(b) = args.next() {
                                                match graph.find_vert(a) {
                                                    Ok(a) => {
                                                        match graph.find_vert(b) {
                                                            Ok(b) => {
                                                                if graph.adjacent[a as usize].iter().any(|Adjacent { vertex, .. }| vertex == &b) {
                                                                    write_cout!(@reply: console, Warning, "vertices {a} and {b} are already connected");
                                                                } else {
                                                                    graph.add_edge(a, b);
                                                                    route = None;
                                                                    write_cout!(@reply: console, Info, "created an edge connecting vertices {a} and {b}");
                                                                }
                                                            }
                                                            Err(id) => write_cout!(@reply: console, Error, "vertex \"{id}\" does not exist"),
                                                        }
                                                    }
                                                    Err(id) => write_cout!(@reply: console, Error, "vertex \"{id}\" does not exist"),
                                                }
                                            } else { check_usage = true; }
                                        } else { check_usage = true; }
                                    }

                                    CMD_TEMPO => {
                                        if let Some(arg) = args.next() {
                                            let mut is_changed = false;
                                            if arg == "reset" {
                                                tempo_ticks = 1;
                                                tempo_ms = 16;
                                                is_changed = true;
                                            } else if arg == "sprint" {
                                                tempo_ticks = 1;
                                                tempo_ms = 0;
                                                is_changed = true;
                                            } else if let Some((ticks, ms)) = arg.split_once('/') {
                                                let ticks = ticks.parse().inspect_err(|e| write_cout!(@reply: console, Error, "ticks (\"{ticks}\"): {e}"));
                                                let ms = ms.parse().inspect_err(|e| write_cout!(@reply: console, Error, "ms (\"{ms}\"): {e}"));
                                                if let (Ok(ticks), Ok(ms)) = (ticks, ms) {
                                                    (tempo_ticks, tempo_ms) = (ticks, ms);
                                                    is_changed = true;
                                                }
                                            } else { check_usage = true; }
                                            if is_changed {
                                                write_cout!(@reply: console, Info, "set tempo to {tempo_ticks} steps every {tempo_ms}ms");
                                            }
                                        } else {
                                            write_cout!(@reply: console, Info, "current tempo is {tempo_ticks} steps every {tempo_ms}ms");
                                        }
                                    }

                                    CMD_DBG => {
                                        is_debugging = !is_debugging;
                                        write_cout!(@reply: console, Info, "debugging is now {}", if is_debugging { "on" } else { "off" });
                                    }

                                    CMD_FOCUS => {
                                        if let Some(target) = args.next() {
                                            if target == "reset" {
                                                camera.position = CAMERA_POSITION_DEFAULT;
                                                camera.target = Vector3::zero();
                                            } else {
                                                let vert = graph.verts.iter()
                                                    .find(|vert| (vert.id.eq_ignore_ascii_case(target) || vert.alias.eq_ignore_ascii_case(target)));

                                                if let Some(vert) = vert {
                                                    camera.target = vert.pos;
                                                    camera.position = camera.target + Vector3::new(0.0, 400.0, 0.0);
                                                } else {
                                                    write_cout!(@reply: console, Error, "vertex \"{target}\" does not exist");
                                                }
                                            }
                                        } else {
                                            let vert = graph.verts.iter()
                                                .find(|vert| check_collision_spheres(vert.pos, VERTEX_RADIUS, camera.target, 1.0));

                                            if let Some(target) = vert {
                                                write_cout!(@reply: console, Info, "currently focusing vertex {}", target.id);
                                            } else {
                                                write_cout!(@reply: console, Info, "no vertex is currently focused");
                                            }
                                        }
                                    }

                                    CMD_CLOSE => {
                                        break 'window;
                                    }
                                }
                                if check_usage {
                                    let usage = cmd.usage();
                                    let has_multiple = usage.len() > 1;
                                    write_cout!(@reply: console, Error, "usage:{}{}", if has_multiple { "\n    " } else { " " }, usage.join("\n    "));
                                }
                            }

                            Err(cmd) => {
                                write_cout!(@reply: console, Error, "no such command: `{cmd}`");
                            }
                        }
                    }
                }
            }
        }

        if rl.is_key_released(KeyboardKey::KEY_BACKSPACE) {
            backspace_pressed = None;
        }

        if is_giving_command {
            let mut force_blink = true;
            if let Some(ch) = rl.get_char_pressed() {
                console.command.push(ch);
            } else if rl.is_key_pressed(KeyboardKey::KEY_BACKSPACE) {
                backspace_pressed = Some(Instant::now());
                console.command.pop();
            } else if let Some(pressed_time) = &mut backspace_pressed {
                const DELAY: Duration = Duration::from_millis(550);
                const REP: Duration = Duration::from_millis(33);
                if pressed_time.elapsed() >= DELAY {
                    *pressed_time = Instant::now() - DELAY + REP;
                    console.command.pop();
                }
            } else if rl.is_key_pressed(KeyboardKey::KEY_UP) {
                console.command = command_history.get(command_history_offset as usize).cloned().unwrap_or_default();
                command_history_offset = (command_history_offset + 1).min(command_history.len() as isize);
            } else if rl.is_key_pressed(KeyboardKey::KEY_DOWN) {
                command_history_offset = (command_history_offset - 1).max(-1);
                console.command = command_history.get(command_history_offset as usize).cloned().unwrap_or_default();
            } else {
                force_blink = false;
            }

            const BLINK_TIME: Duration = Duration::from_millis(500);
            if force_blink {
                is_cursor_shown = true;
                cursor_last_toggled = Instant::now() - BLINK_TIME / 2;
            } else if cursor_last_toggled.elapsed() >= BLINK_TIME {
                is_cursor_shown = !is_cursor_shown;
                cursor_last_toggled = Instant::now();
            }
        } else {
            let speed = 4.0*camera.position.distance_to(camera.target)/CAMERA_POSITION_DEFAULT.y;
            let north = (rl.is_key_down(KeyboardKey::KEY_W) as i8 - rl.is_key_down(KeyboardKey::KEY_S) as i8) as f32*speed;
            let east  = (rl.is_key_down(KeyboardKey::KEY_D) as i8 - rl.is_key_down(KeyboardKey::KEY_A) as i8) as f32*speed;
            let up    = (rl.is_key_down(KeyboardKey::KEY_Q) as i8 - rl.is_key_down(KeyboardKey::KEY_E) as i8) as f32*speed;
            let pitch = (rl.is_key_down(KeyboardKey::KEY_R) as i8 - rl.is_key_down(KeyboardKey::KEY_F) as i8) as f32*speed;
            let yaw   = (rl.is_key_down(KeyboardKey::KEY_X) as i8 - rl.is_key_down(KeyboardKey::KEY_Z) as i8) as f32*speed;
            let pan = Vector3::new(east, 0.0, -north);
            let zoom = Vector3::new(yaw, up, -pitch);
            camera.position += pan + zoom;
            camera.target += pan;
        }

        if let Some((route, last_step)) = &mut route {
            if !route.is_finished {
                if let Some(tempo_ms) = std::num::NonZeroU128::new(tempo_ms) {
                    let ms_elapsed = last_step.elapsed().as_millis();
                    let ticks = tempo_ticks * ms_elapsed/tempo_ms;
                    println!("tempo_ticks: {tempo_ticks} tempo_ms: {tempo_ms} ms_elapsed: {ms_elapsed} ticks: {ticks}");
                    for _ in 0..ticks {
                        if route.is_finished { break; }
                        route.step(&mut console);
                        *last_step = Instant::now();
                    }
                } else {
                    while !route.is_finished {
                        route.step(&mut console);
                        *last_step = Instant::now();
                    }
                };
            }
        }

        let route = &route;

        let mut d = rl.begin_drawing(&thread);
        d.clear_background(Color::new(8, 0, 0, 255));

        {
            const SCALE_FACTOR: f32 = 1.0/10.0;
            let mut d = d.begin_mode3D({
                let mut camera = camera;
                camera.target *= SCALE_FACTOR;
                camera.position *= SCALE_FACTOR;
                camera
            });

            for edge in &graph.edges {
                let [i, j] = edge.adj.map(usize::from);
                let p0 = graph.verts[i].pos;
                let p1 = graph.verts[j].pos;
                d.draw_capsule(p0*SCALE_FACTOR, p1*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::RED.alpha(0.25));
            }

            for (v, vert) in graph.verts_iter() {
                let distance_from_target = vert.pos - camera.target;
                let color = loop {
                    if let Some((route, _)) = &route {
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
                        }
                    }
                    let dist_sqr = distance_from_target.dot(distance_from_target);
                    if dist_sqr <= VERTEX_RADIUS*VERTEX_RADIUS {
                        break Color::new(255, 128, 128, 255);
                    } else {
                        break Color::RED;
                    }
                };
                let resolution = lerp(24.0, 8.0, (camera.position.distance_to(vert.pos)/1000.0).clamp(0.0, 1.0)).round() as i32; // LOD
                d.draw_sphere_ex(vert.pos*SCALE_FACTOR, VERTEX_RADIUS*SCALE_FACTOR, resolution, resolution, color);
                if let Some((route, _)) = &route {
                    if let Some(Visit { parent: Some(p), .. }) = route.visited[v as usize] {
                        d.draw_capsule(graph.verts[p as usize].pos*SCALE_FACTOR, vert.pos*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::ORANGERED);
                    }
                }
            }

            if let Some((route, _)) = &route {
                for pair in route.result.windows(2) {
                    let [a, b] = pair else { panic!("window(2) should always create 2 elements") };
                    d.draw_capsule(graph.verts[*a as usize].pos*SCALE_FACTOR, graph.verts[*b as usize].pos*SCALE_FACTOR, 2.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
                }
            }

            d.draw_capsule((camera.target + Vector3::new(-5.0, 0.0,  0.0))*SCALE_FACTOR, (camera.target + Vector3::new(5.0, 0.0, 0.0))*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
            d.draw_capsule((camera.target + Vector3::new( 0.0, 0.0, -5.0))*SCALE_FACTOR, (camera.target + Vector3::new(0.0, 0.0, 5.0))*SCALE_FACTOR, 1.0*SCALE_FACTOR, 16, 0, Color::BLUEVIOLET);
        }

        for (v, vert) in graph.verts_iter() {
            let pos = d.get_world_to_screen(vert.pos, camera);
            let text = vert.alias.as_str();
            let text_size = {
                let c_text = std::ffi::CString::new(text).unwrap();
                unsafe { ffi::MeasureTextEx(*font.as_ref(), c_text.as_ptr(), font.baseSize as f32, 0.0) }
            };
            d.draw_text_ex(&font, text, pos - rvec2(text_size.x*0.5, font.baseSize/2), font.baseSize as f32, 0.0, Color::WHITE);
            if let Some((route, _)) = &route {
                if let Some(Visit { distance, parent }) = route.visited[v as usize] {
                    let parent_text = parent.map_or("-", |p| &graph.verts[p as usize].alias);
                    let text = format!("{} ({parent_text})", distance.ceil());
                    d.draw_text_ex(&font, &text, pos + rvec2(text_size.x*0.5 + 3.0, 3), font.baseSize as f32, 0.0, Color::GRAY);
                }
            }
        }

        // Console

        // console.route = route.result.iter()
        //     .map(|&v| graph.verts[v as usize].id.as_str())
        //     .collect::<Vec<&str>>()
        //     .join(" - ");

        // if !is_giving_command {
            // let target_text = route.targets.iter()
            //     .map(|&v| graph.verts[v as usize].id.as_str())
            //     .collect::<Vec<&str>>()
            //     .join(", ");
            // console.target = Some(target_text);
        // }

        if !is_giving_command {
            d.draw_text_ex(&font,
                "use WASD to pan and RFZX to orbit",
                rvec2(0, d.get_render_height() - font.baseSize*3), font.baseSize as f32, 0.0, Color::GREENYELLOW);
        }

        d.draw_text_ex(&font,
            &if is_giving_command {
                format!("press ENTER to run the command, or ESCAPE to cancel")
            } else {
                format!("press ENTER to begin typing a command")
            },
            rvec2(0, d.get_render_height() - font.baseSize*2), font.baseSize as f32, 0.0, Color::GREENYELLOW);

        d.draw_text_ex(&font, &format!("x:{}/y:{}", camera.target.x, camera.target.z), rvec2(0, d.get_render_height() - font.baseSize), font.baseSize as f32, 0.0, Color::GREENYELLOW);

        let mut n = 0;
        for item in console.iter() {
            let (color, prefix, suffix) = match item.cat {
                ConsoleLineCategory::Route                 => (Color::RAYWHITE,  "route: ", ""),
                ConsoleLineCategory::Command               => (Color::LIGHTBLUE, ">", if is_giving_command && is_cursor_shown { "_" } else { "" }),
                ConsoleLineCategory::TargetList            => (Color::LIME,      "targets: ", ""),
                ConsoleLineCategory::Trace                 => (Color::DARKGRAY,  "trace: ", ""),
                ConsoleLineCategory::Debug if is_debugging => (Color::MAGENTA,   "debug: ", ""),
                ConsoleLineCategory::Debug                 => continue,
                ConsoleLineCategory::Info                  => (Color::LIGHTGRAY, "", ""),
                ConsoleLineCategory::Warning               => (Color::GOLD,      "warning: ", ""),
                ConsoleLineCategory::Error                 => (Color::RED,       "err: ", ""),
                ConsoleLineCategory::Fatal                 => (Color::SALMON,    "fatal: ", ""),
            };
            for line in format!("{prefix}{}{suffix}", item.msg).lines() {
                d.draw_text_ex(&font, &line, rvec2(0, font.baseSize*n), font.baseSize as f32, 0.0, color);
                n += 1;
            }
        }
    }
}
