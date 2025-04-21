use std::{collections::VecDeque, time::Instant};
use crate::{console::{console_debug, Console}, graph::{Adjacent, VertexID, WeightedGraph}};

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
    pub distance: f32,
    pub parent: Option<VertexID>,
}

pub struct RouteGenerator {
    pub targets: Vec<VertexID>,
    pub result: Vec<VertexID>,
    pub root: VertexID,
    pub visited: Box<[Option<Visit>]>,
    pub queue: VecDeque<VertexID>,
    pub phase: Phase,
    pub is_finished: bool,
    pub last_step: Instant,
}

impl RouteGenerator {
    pub fn new(num_verts: usize, start: VertexID, targets: impl IntoIterator<Item = VertexID>) -> Self {
        Self {
            targets: Vec::from_iter(targets),
            result: vec![start],
            root: start,
            visited: vec![const { None }; num_verts].into_boxed_slice(),
            queue: VecDeque::new(),
            phase: Phase::None,
            is_finished: false,
            last_step: Instant::now(),
        }
    }

    fn begin_phase_edge(&mut self, console: &mut Console) {
        console_debug!(console, Info, 1, "edge phase");
        console_debug!(console, Info, 2, "looking at vertex {}", self.root);
        self.queue.clear();
        self.visited.fill(const { None });
        self.visited[self.root as usize] = Some(Visit { distance: 0.0, parent: None });
        self.phase = Phase::Edge {
            current: self.root,
            i: 0,
        };
    }

    fn begin_phase_target(&mut self, console: &mut Console) {
        console_debug!(console, Info, 1, "target phase");
        self.phase = Phase::Target {
            shortest_distance: f32::INFINITY,
            nearest_target: usize::MAX,
            i: 0,
        };
    }

    fn begin_phase_backtrack(&mut self, console: &mut Console) {
        console_debug!(console, Info, 1, "backtrack phase");
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

    pub fn step(&mut self, console: &mut Console, graph: &WeightedGraph) {
        debug_assert!(!self.is_finished, "do not continue finished route");
        match &mut self.phase {
            Phase::None => {
                console_debug!(console, Info, 0, "root set to vertex {}; targeting {:?}", self.root, self.targets);
                self.begin_phase_edge(console);
            }

            Phase::Edge { current, i } => {
                if let Some(&Adjacent { vertex, weight }) = graph.adjacent[*current as usize].get(*i) {
                    console_debug!(console, Info, 2, "looking at vertex {current} ({i}/{})", graph.adjacent[*current as usize].len());
                    if self.visited[vertex as usize].is_none() {
                        self.queue.push_back(vertex);
                        console_debug!(console, Info, 3, "pushed vertex {vertex} to queue");
                        for &Adjacent { vertex, .. } in &graph.adjacent[vertex as usize] {
                            if !self.queue.contains(&vertex) {
                                self.queue.push_back(vertex);
                                console_debug!(console, Info, 3, "pushed vertex {vertex} to queue");
                            }
                        }
                    }
                    let distance = self.visited[*current as usize].expect("current must have been visited if it is queued").distance + weight;
                    console_debug!(console, Info, 3, "vertex {vertex} is {distance} from root (vertex {}) through vertex {current}", self.root);
                    if self.visited[vertex as usize].is_none_or(|visit| distance < visit.distance) {
                        self.visited[vertex as usize] = Some(Visit { distance, parent: Some(*current) });
                        console_debug!(console, Info, 4, "new best");
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
                        console_debug!(console, Info, 2, "target {} (vertex {v}) is {distance} from root (vertex {})", *i, self.root);
                        if distance < *shortest_distance {
                            console_debug!(console, Info, 3, "updating nearest target to {} (vertex {v})", *i);
                            *shortest_distance = distance;
                            *nearest_target = *i;
                        }
                        *i += 1;
                    } else {
                        console_debug!(console, Error, 0, "no route exists");
                        self.is_finished = true;
                    }
                } else {
                    assert!(*nearest_target < self.targets.len());
                    self.root = self.targets.swap_remove(*nearest_target);
                    console_debug!(console, Info, 2, "nearest target identified as vertex {}", self.root);
                    self.begin_phase_backtrack(console);
                }
            }

            Phase::Backtrack { parent, insert_at, i } => {
                if let Some(Visit { parent: Some(p), .. }) = &self.visited[*parent as usize] {
                    self.result.insert(*insert_at, *parent);
                    *parent = *p;
                } else {
                    console_debug!(console, Info, 2, "adding vertex {} to results", self.root);
                    if self.targets.is_empty() {
                        let final_route = self.result.iter()
                            .map(|&v| graph.verts[v as usize].id.as_str())
                            .collect::<Vec<&str>>()
                            .join(" - ");
                        console.reply.clear();
                        console_debug!(console, Route, 0, "{final_route}");
                        self.is_finished = true;
                        let mut distance = 0.0;
                        self.visited.fill(const { None });
                        self.visited[self.result[0] as usize] = Some(Visit { distance, parent: None });
                        if *i + 1 < self.result.len() {
                            let a = &self.result[*i];
                            let b = &self.result[*i + 1];
                            distance += graph.adjacent[*a as usize].iter()
                                .find_map(|e| (&e.vertex == b).then_some(e.weight))
                                .expect("results should be adjacent");
                            self.visited[*b as usize] = Some(Visit { distance, parent: Some(*a) });
                            *i += 1;
                        }
                        console_debug!(console, Route, 1, "total distance: {distance}");
                    } else {
                        self.begin_phase_edge(console);
                    }
                }
            }
        }
        self.last_step = Instant::now();
    }
}