use std::{ops::ControlFlow, str::FromStr, task::Poll};
use raylib::prelude::*;
// use snippet::Snippet;
use crate::{camera::Orbiter, console::{input::ConsoleIn, output::ConsoleOut}, console_log, graph::{VertexID, WeightedGraph}, route::RouteGenerator, serialization::LoadGraphError, types::{Coords, ParseColorError, ParseCoordsError, ParseTempoError, RichColor, Tempo}, CAMERA_LENGTH_DEFAULT, VERTEX_RADIUS};

pub mod snippet;
// pub mod exec;
// mod cmd;

/// All information that can be affected by commands
pub struct ProgramData {
    pub graph: WeightedGraph,
    pub route: Option<RouteGenerator>,
    pub is_debugging: bool,
    pub orbit: Orbiter,
    pub tempo: Tempo,
    pub interactive_targets: Vec<VertexID>,
    pub is_giving_interactive_targets: bool,
    pub should_close: bool,
    pub verts_color: Color,
    pub edges_color: Color,
    pub background_color: Color,
}

pub struct CmdReturn {
    rets: Vec<String>,
    disp: Option<CmdRetDisplay>,
}

impl Default for CmdReturn {
    fn default() -> Self {
        Self::void()
    }
}

impl CmdReturn {
    const fn void() -> Self {
        Self { rets: Vec::new(), disp: None }
    }

    const fn pure(rets: Vec<String>) -> Self {
        Self { rets, disp: None }
    }

    fn disp(disp: CmdRetDisplay) -> Self {
        Self { rets: Vec::new(), disp: Some(disp) }
    }

    fn new(rets: Vec<String>, disp: CmdRetDisplay) -> Self {
        Self { rets, disp: Some(disp) }
    }

    pub fn print(self, cout: &mut ConsoleOut, data: &ProgramData) {
        if let Some(disp) = self.disp {
            disp.display(cout, data, self.rets);
        }
    }
}

macro_rules! define_commands {
    (
        $(
            #[input = $input:literal]
            #[help = $help:literal]
            $(#[$meta:meta])*
            $Command:ident {
                $(
                    #[template = $template:literal]
                    #[help = $usage_help:literal]
                    #[rets($($RetDisp:ident$({ ($rd_fields:pat): $RetDispFields:tt })?
                        = |$rd_cout:ident, $rd_data:ident, $rd_args:ident| $ret_disp:expr),* $(,)?)]
                    $(#[$usage_meta:meta])*
                    $Usage:ident = |$cout:ident, $cin:ident, $data:ident, $args:ident| $def:expr
                ),+ $(,)?
            }
        ),+ $(,)?
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Cmd {
            $($Command),*
        }

        impl FromStr for Cmd {
            type Err = CmdError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($input => Ok(Self::$Command),)+
                    _ => Err(CmdError::NoSuchCmd(s.to_string())),
                }
            }
        }

        impl Cmd {
            pub const LIST: [Self; [$(Self::$Command),+].len()] = [$(Self::$Command),+];

            pub const fn usage_list(self) -> &'static [Usage] {
                match self {
                    $(Self::$Command => &[$(Usage::$Command(cmd::$Command::$Usage)),+]),+
                }
            }

            pub const fn input(self) -> &'static str {
                match self {
                    $(Self::$Command => $input),+
                }
            }

            pub const fn help(self) -> &'static str {
                match self {
                    $(Self::$Command => $help),+
                }
            }

            pub fn run(
                self,
                cout: &mut ConsoleOut,
                cin: &mut ConsoleIn,
                data: &mut ProgramData,
                args: &[&str],
            ) -> Result<CmdPromise, CmdError> {
                match self { $(Self::$Command => cmd::$Command::RUNNERS.as_slice()),+ }
                    .iter()
                    .find_map(|runner| match runner(cout, cin, data, args) {
                        Err(CmdError::CheckUsage(_)) => None,
                        ret => Some(ret),
                    })
                    .unwrap_or_else(|| Err(CmdError::CheckUsage(self)))
            }
        }
        pub type CmdRunner = fn(&mut ConsoleOut, &mut ConsoleIn, &mut ProgramData, &[&str]) -> Result<CmdPromise, CmdError>;

        mod cmd {
            use super::*;
            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                pub enum $Command {
                    $($Usage),+
                }
                impl $Command {
                    pub const RUNNERS: [CmdRunner; const { [$(Self::$Usage),+].len() }] = [$(|$cout, $cin, $data, $args| $def,)+];

                    #[allow(dead_code, reason = "not all are used")]
                    #[inline]
                    pub const fn runner(self) -> CmdRunner {
                        Self::RUNNERS[self as usize]
                    }
                }
            )+
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Usage {
            $($Command(cmd::$Command),)+
        }

        impl Usage {
            pub const fn template(self) -> &'static str {
                match self {
                    $($(Self::$Command(cmd::$Command::$Usage) => $template,)+)+
                }
            }

            pub const fn help(self) -> &'static str {
                match self {
                    $($(Self::$Command(cmd::$Command::$Usage) => $usage_help,)+)+
                }
            }
        }

        pub enum CmdRetDisplay {
            $($($($RetDisp$($RetDispFields)?,)*)+)+
        }

        impl CmdRetDisplay {
            pub fn display(self, cout: &mut ConsoleOut, data: &ProgramData, args: Vec<String>) {
                match self {
                    $($($(
                        Self::$RetDisp$(($rd_fields))? => {
                            let ($rd_cout, $rd_data, $rd_args) = (cout, data, args);
                            $ret_disp
                        }
                    )*)+)+
                }
            }
        }
    };
}

define_commands!{
    #[input = "help"]
    #[help = "Display information about commands."]
    Help {
        #[template = ""]
        #[help = "Display general information about all commands."]
        #[rets(HelpAll = |cout, _data, _args| console_log!(cout, Info, "{}", help_all_msg()))]
        All = |_cout, _cin, _data, args| {
            if args.is_empty() {
                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::HelpAll)))
            } else {
                Err(CmdError::CheckUsage(Cmd::Help))
            }
        },

        #[template = "<COMMAND>"]
        #[help = "Display more specific information about a specific command."]
        #[rets(HelpOne{(cmd): (Cmd)} = |cout, _data, _args| console_log!(cout, Info, "{}", cmd.help_msg()))]
        One = |_cout, _cin, _data, args| {
            if let [cmd] = args {
                let cmd = cmd.parse::<Cmd>()
                    .map_err(|_| CmdError::CheckUsage(Cmd::Help))?;

                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::HelpOne(cmd))))
            } else {
                Err(CmdError::CheckUsage(Cmd::Help))
            }
        },
    },

    #[input = "quit"]
    #[help = "Close the application."]
    Quit {
        #[template = ""]
        #[help = "Close the application immediately."]
        #[rets()]
        Basic = |_cout, _cin, data, args| {
            if args.is_empty() {
                data.should_close = true;
                Ok(CmdPromise::Ready(CmdReturn::void()))
            } else {
                Err(CmdError::CheckUsage(Cmd::Cls))
            }
        },
    },
    #[input = "cls"]
    #[help = "Clear the console."]
    Cls {
        #[template = ""]
        #[help = "Clear the console history text."]
        #[rets()]
        Basic = |cout, _cin, _data, args| {
            if args.is_empty() {
                cout.clear_log();
                Ok(CmdPromise::Ready(CmdReturn::void()))
            } else {
                Err(CmdError::CheckUsage(Cmd::Cls))
            }
        },
    },
    #[input = "echo"]
    #[help = "Repeat the arguments."]
    Echo {
        #[template = "[ANY]..."]
        #[help = "Print the argument(s) or pass them as arguments to the next command in the pipeline."]
        #[rets(Echo = |_cout, _data, _args| {})]
        Basic = |_cout, _cin, _data, args| {
            Ok(CmdPromise::Ready(CmdReturn::new(args.iter().map(<&str>::to_string).collect(), CmdRetDisplay::Echo)))
        },
    },
    #[input = "dbg"]
    #[help = "Toggle debug messages."]
    Dbg {
        #[template = ""]
        #[help = "Toggle whether debug messages are displayed."]
        #[rets(Dbg = |cout, data, _args| console_log!(cout, Info, "debug messages are now {}", if data.is_debugging { "enabled" } else { "disabled" }))]
        Toggle = |_cout, _cin, data, args| {
            if args.is_empty() {
                data.is_debugging = !data.is_debugging;
                Ok(CmdPromise::Ready(CmdReturn::new(vec![if data.is_debugging { "true" } else { "false" }.to_string()], CmdRetDisplay::Dbg)))
            } else {
                Err(CmdError::CheckUsage(Cmd::Dbg))
            }
        },

        #[template = "true|false"]
        #[help = "Enable or disable the display of debug messages."]
        #[rets()]
        Set = |_cout, _cin, data, args| {
            if let [set_arg] = args {
                data.is_debugging = set_arg.parse::<bool>()
                    .map_err(CmdError::ParseBool)?;

                Ok(CmdPromise::Ready(CmdReturn::new(vec![if data.is_debugging { "true" } else { "false" }.to_string()], CmdRetDisplay::Dbg)))
            } else {
                Err(CmdError::CheckUsage(Cmd::Dbg))
            }
        },
    },
    #[input = "focus"]
    #[help = "Focus a vertex."]
    Focus {
        #[template = "<ID|ALIAS>"]
        #[help = "Pan and zoom the camera to focus the specified vertex."]
        #[rets(FocusVertex = |cout, _data, args| console_log!(cout, Info, "focused vertex {}", args[0]))]
        Vertex = |_cout, _cin, data, args| {
            if let [target_arg] = args && &**target_arg != "reset" {
                let vert = data.graph.verts().iter()
                    .find(|vert| (vert.id.eq_ignore_ascii_case(target_arg) || vert.alias.eq_ignore_ascii_case(target_arg)))
                    .ok_or_else(|| CmdError::VertexDNE(target_arg.to_string()))?;

                data.orbit.target = vert.pos;
                data.orbit.length = 400.0;
                Ok(CmdPromise::Ready(CmdReturn::new(vec![vert.alias.clone()], CmdRetDisplay::FocusVertex)))
            } else {
                Err(CmdError::CheckUsage(Cmd::Focus))
            }
        },

        #[template = "reset"]
        #[help = "Reset the pan and zoom to the original orientation."]
        #[rets()]
        Reset = |_cout, _cin, data, args| {
            if matches!(args, ["reset"]) {
                data.orbit.target = Vector3::ZERO;
                data.orbit.length = CAMERA_LENGTH_DEFAULT;
                Ok(CmdPromise::Ready(CmdReturn::void()))
            } else {
                Err(CmdError::CheckUsage(Cmd::Focus))
            }
        },

        #[template = ""]
        #[help = "Print the ID of the currently focused vertex or pass it as an argument to the next command in the pipeline."]
        #[rets(FocusPrint = |cout, _data, args| {
            if args.is_empty() {
                console_log!(cout, Info, "no vertex is currently focused");
            } else {
                console_log!(cout, Info, "currently focusing vertex {}", args[0]);
            }
        })]
        Print = |_cout, _cin, data, args| {
            if args.is_empty() {
                let vert = data.graph.verts().iter()
                    .find(|vert| check_collision_spheres(vert.pos, VERTEX_RADIUS, data.orbit.target, 1.0));

                Ok(CmdPromise::Ready(CmdReturn::new(vert.into_iter().map(|target| target.id.clone()).collect::<Vec<_>>(), CmdRetDisplay::FocusPrint)))
            } else {
                Err(CmdError::CheckUsage(Cmd::Focus))
            }
        },
    },
    #[input = "color.verts"]
    #[help = "Set the color of all vertices."]
    ColorVerts {
        #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
        #[help = "Set the base color used to render all vertices."]
        #[rets(ColorVertsGeneral = |cout, _data, _args| console_log!(cout, Info, "updated vertex color"))]
        General = |_cout, _cin, data, args| {
            if let [color_arg] = args {
                data.verts_color = color_arg.parse::<RichColor>()
                    .map(|RichColor(c)| c)
                    .map_err(CmdError::ParseColor)?;
                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::ColorVertsGeneral)))
            } else {
                Err(CmdError::CheckUsage(Cmd::ColorVerts))
            }
        },

        #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME> <ID|ALIAS>..."]
        #[help = "[UNDER CONSTRUCTION] Set the base color used to render the specified vertices."]
        #[rets()]
        Specific = |_cout, _cin, _data, _args| todo!("run_color_verts_specific requires per-vertex colors"),
    },
    #[input = "color.edges"]
    #[help = "Set the color of all edges."]
    ColorEdges {
        #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
        #[help = "Set the base color used to render all edges."]
        #[rets(ColorEdges = |cout, _data, _args| console_log!(cout, Info, "updated edge color"))]
        Basic = |_cout, _cin, data, args| {
            if let [color_arg] = args {
                data.edges_color = color_arg.parse::<RichColor>()
                    .map(|RichColor(c)| c)
                    .map_err(CmdError::ParseColor)?;
                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::ColorEdges)))
            } else {
                Err(CmdError::CheckUsage(Cmd::ColorVerts))
            }
        },
    },
    #[input = "color.bg"]
    #[help = "Set the color of the background."]
    ColorBackground {
        #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
        #[help = "Set the base color used to render background."]
        #[rets(ColorBackground = |cout, _data, _args| console_log!(cout, Info, "updated background color"))]
        Basic = |_cout, _cin, data, args| {
            if let [color_arg] = args {
                data.background_color = color_arg.parse::<RichColor>()
                    .map(|RichColor(c)| c)
                    .map_err(CmdError::ParseColor)?;
                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::ColorBackground)))
            } else {
                Err(CmdError::CheckUsage(Cmd::ColorVerts))
            }
        },
    },
    #[input = "sv.route"]
    #[help = "Generate a route."]
    SvRoute {
        #[template = "<ID|ALIAS> <ID|ALIAS>..."]
        #[help = "Begin generating a route starting from the first vertex and visiting every following vertex in an order the minimizes total distance travelled."]
        #[rets(SvRouteImmediate = |cout, _data, args|
            console_log!(cout, Route, "{}", args.join(" - ")),
            // let mut distance = 0.0;
            // route.visited.fill(const { None });
            // route.visited[results[0] as usize] = Some(Visit { distance, parent: None });
            // if *i + 1 < results.len() {
            //     let a = &results[*i];
            //     let b = &results[*i + 1];
            //     distance += graph.adjacent(*a).iter()
            //         .find_map(|e| (&e.vertex == b).then_some(e.weight))
            //         .expect("results should be adjacent");
            //     route.visited[*b as usize] = Some(Visit { distance, parent: Some(*a) });
            //     *i += 1;
            // }
            // console_log!(cout, Route, "total distance: {distance}");
        )]
        Immediate = |cout, _cin, data, args| {
            if let [start, _target, ..] = args && !matches!(&**start, "-i"|"--interactive"|"interactive") {
                let start = start.parse_vert(&data.graph)?;
                let targets = args.iter()
                    .map(|s| s.parse_vert(&data.graph))
                    .collect::<Result<Vec<VertexID>, CmdError>>()?;

                data.route = Some(RouteGenerator::new(data.graph.verts().len(), start, targets));
                console_log!(cout, Info, "generating route...");
                Ok(CmdPromise::Route)
            } else {
                Err(CmdError::CheckUsage(Cmd::SvRoute))
            }
        },

        #[template = "-i|interactive"]
        #[help = "Interactively provide targets to generate a route with by clicking on vertices in the visualizer with your mouse. \n\
                  Run the command `sv.route` without arguments when you are finished selecting targets."]
        #[rets()]
        Interactive = |cout, cin, data, args| {
            if matches!(args, ["-i"|"interactive"]) {
                console_log!(cout, Info,
                    "click each target with the mouse; order doesn't matter except that the first will be the start \n\
                    click a target again to un-target it\n\
                    run the command `<color = #0096e6>sv.route</color>` (without arguments) when finished");
                cin.insert_over_selection("sv.route");
                data.is_giving_interactive_targets = true;
                Ok(CmdPromise::InteractiveTargets)
            } else {
                Err(CmdError::CheckUsage(Cmd::SvRoute))
            }
        },

        #[template = ""]
        #[help = "Submit the targets selected with `sv.route interactive` and begin generating a route."]
        #[rets()]
        Submit = |_cout, _cin, data, args| {
            if args.is_empty() {
                data.is_giving_interactive_targets = false;
                Ok(CmdPromise::Ready(CmdReturn::void()))
            } else {
                Err(CmdError::CheckUsage(Cmd::SvRoute))
            }
        },
    },
    #[input = "sv.route.add"]
    #[help = "Add targets to the current route."]
    SvRouteAdd {
        #[template = ""]
        #[help = "Add more targets for the current route to visit. If any have already been visited, the generator will target them again \
                  as if the current final result is the start of a new route."]
        #[rets()]
        Immediate = |cout, _cin, data, args| {
            if !args.is_empty() && !matches!(args, ["-i"|"interactive", ..]) {
                if let Some(route) = &mut data.route {
                    let targets = args.iter()
                        .map(|s| s.parse_vert(&data.graph))
                        .collect::<Result<Vec<VertexID>, CmdError>>()?;

                    route.add_targets(cout, targets);
                    console_log!(cout, Info, "generating route...");
                    Ok(CmdPromise::Route)
                } else {
                    Err(CmdError::NoExistingRoute)
                }
            } else {
                Err(CmdError::CheckUsage(Cmd::SvRoute))
            }
        },

        #[template = "-i|interactive"]
        #[help = "Interactively provide targets to generate a route with by clicking on vertices in the visualizer with your mouse. \
                  Run the command `sv.route` without arguments when you are finished selecting targets."]
        #[rets()]
        Interactive = |_cout, _cin, data, args| {
            if matches!(args, ["-i"|"--interactive"|"interactive"]) {
                data.is_giving_interactive_targets = true;
                Ok(CmdPromise::InteractiveTargets)
            } else {
                Err(CmdError::CheckUsage(Cmd::SvRouteAdd))
            }
        },
    },
    #[input = "sv.route.list"]
    #[help = "List the results of the current route."]
    SvRouteList {
        #[template = ""]
        #[help = "Print the results of the current route in order or pass them as arguments to the next command in the pipeline."]
        #[rets(SvRouteList = |cout, _data, args| console_log!(cout, Info, "current route: {}", args.join(", ")))]
        Basic = |_cout, _cin, data, args| {
            if args.is_empty() {
                if let Some(route) = &data.route {
                    let list = route
                        .result()
                        .iter()
                        .copied()
                        .map(|id| data.graph.vert(id).id.clone())
                        .collect();
                    Ok(CmdPromise::Ready(CmdReturn::new(list, CmdRetDisplay::SvRouteList)))
                } else {
                    Err(CmdError::NoExistingRoute)
                }
            } else {
                Err(CmdError::CheckUsage(Cmd::SvRouteList))
            }
        },
    },
    #[input = "sv.route.clear"]
    #[help = "Clear the current route."]
    SvRouteClear {
        #[template = ""]
        #[help = "Remove all targets, results, and visuals from the current route."]
        #[rets(SvRouteClear = |cout, _data, _args| console_log!(cout, Info, "route cleared"))]
        Basic = |_cout, _cin, data, args| {
            if args.is_empty() {
                data.route = None;
                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::SvRouteClear)))
            } else {
                Err(CmdError::CheckUsage(Cmd::SvRouteClear))
            }
        },
    },
    #[input = "sv.new"]
    #[help = "Create a new vertex."]
    SvNew {
        #[template = "<ID> [ALIAS] x:<???>/y:<???>[/z:<???>]"]
        #[help = "Create a new vertex with the specified name/alias at the specified position. It will start with no edges. The alias can be the same as the ID, \
                  but neither the ID nor alias may match the ID of any existing vertex, nor may either match the alias of any existing vertex."]
        #[rets(SvNew = |cout, _data, _args| console_log!(cout, Info, "created vertex"))]
        Basic = |_cout, _cin, data, args| {
            let (id, alias, pos) = match *args {
                [id, alias, pos] => (id, alias, pos),
                [id, pos] => (id, id, pos),
                _ => return Err(CmdError::CheckUsage(Cmd::SvNew)),
            };
            let Coords(pos) = pos.parse()
                .map_err(CmdError::ParseCoords)?;

            let id_and_alias = [id, alias];
            let id_and_alias_unique = &id_and_alias[0..=(id != alias) as usize];

            // invalid name
            for name in id_and_alias_unique {
                if let Some(reserved) = ReservedName::find(name) {
                    return Err(CmdError::BadVertexName(reserved));
                }
            }

            // duplicate name
            for name in id_and_alias_unique {
                if data.graph.find_vert(name).is_some() {
                    return Err(CmdError::DuplicateVertexName(name.to_string()));
                }
            }

            data.graph.add_vertex(id, alias, pos);

            Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::SvNew)))
        },
    },
    #[input = "sv.edge"]
    #[help = "Create a new edge."]
    SvEdge {
        #[template = "<ID|ALIAS> [--] <ID|ALIAS>..."]
        #[help = "Create two-way edges connecting the prior vertex to each of the following vertices not separated by `--`s.
                  A vertex cannot have an edge to itself and two vertices cannot connect to each other more than once.\
                  When one or more vertices not separated with `--`s are followed by a `--`, each vertex in that subset will be connected to each vertex in the following subset."]
        #[rets(SvEdge = |cout, _data, _args| console_log!(cout, Info, "created edge"))]
        Basic = |_cout, _cin, data, args| {
            if args.len() >= 2 && !matches!(args, ["--", ..] | [.., "--"]) && !args.array_windows::<2>().any(|pair| matches!(pair, ["--", "--"])) {
                let groups = args.split(|arg| *arg == "--")
                    .map(|group|
                        group.iter()
                            .map(|name|
                                data.graph.find_vert(name)
                                    .ok_or_else(|| CmdError::VertexDNE(name.to_string()))
                            )
                            .collect::<Result<Box<[VertexID]>, CmdError>>()
                    )
                    .collect::<Result<Box<[Box<[VertexID]>]>, CmdError>>()?;

                // ex:
                // a -- b => a--b
                // a -- b c => a--b a--c
                // a b -- c => a--c b--c
                // a b -- c d => a--c a--d b--c b--d
                // a b -- c d -- e f => a--c a--d b--c b--d c--e c--f d--e d--f

                let mut e = false;
                for [g1, g2] in groups.array_windows::<2>() {
                    for a in g1.iter().copied() {
                        for b in g2.iter().copied() {
                            if a != b && !data.graph.adjacent(a).iter().any(|adj| adj.vertex == b) {
                                data.graph.add_edge(a, b);
                            } else {
                                e = true;
                            }
                        }
                    }
                }
                if e {
                    Err(CmdError::EdgeLoop) // all valid connections are made, but error at the end because otherwise some might be added while others aren't
                } else {
                    Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::SvEdge)))
                }
            } else {
                Err(CmdError::CheckUsage(Cmd::SvEdge))
            }
        },
    },
    #[input = "sv.load"]
    #[help = "Load a graph from a file."]
    SvLoad {
        #[template = "<PATH>"]
        #[help = "Load a weight graph map from the specified file path."]
        #[rets(SvLoad = |cout, _data, _args| console_log!(cout, Info, "graph loaded"))]
        Basic = |_cout, _cin, data, args| {
            if let [path] = args {
                let s = std::fs::read_to_string(path)
                    .map_err(CmdError::IOError)?;
                data.graph = WeightedGraph::load_from_memory(&s)
                    .map_err(CmdError::LoadGraph)?;
                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::SvLoad)))
            } else {
                Err(CmdError::CheckUsage(Cmd::SvLoad))
            }
        },
    },
    #[input = "sv.save"]
    #[help = "Save the graph to a file."]
    SvSave {
        #[template = "<PATH>"]
        #[help = "Save the current weighted graph map to the specified file path. If the file does not exist, it will be created."]
        #[rets(SvSave = |cout, _data, _args| console_log!(cout, Info, "graph saved"))]
        Basic = |_cout, _cin, data, args| {
            if let [path] = args {
                let s = data.graph.save_to_memory();
                std::fs::write(path, s)
                    .map_err(CmdError::IOError)?;
                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::SvSave)))
            } else {
                Err(CmdError::CheckUsage(Cmd::SvLoad))
            }
        },
    },
    #[input = "tempo"]
    #[help = "Set the tempo of the route generator."]
    Tempo {
        #[template = "reset|sync|sprint|instant|pause|ticks:<???>/ms:<???>"]
        #[help = "Set the rate at which the route generator performs update steps in ticks per millisecond. \
                  Ticks can only occur during frames, so if the 'ms' are lower the duration of a single frame, \
                  multiple ticks will occur during a frame and only the latest one will be visualized in that frame.\n\
                  - `reset`: set the tempo to 1 tick per millisecond.\n\
                  - `sync`: set the tempo to 1 tick per frame.\n\
                  - `sprint`: set the tempo to the fastest possible while keeping the current framerate.\n\
                  - `instant`: set the tempo to 1 tick per 0 milliseconds, blocking until the route is completed.\n\
                  - `pause`: set the tempo to 0 ticks per millisecond."]
        #[rets(TempoSet = |cout, _data, _args| console_log!(cout, Info, "updated tempo"))]
        Set = |_cout, _cin, data, args| {
            if let [arg] = args {
                match arg.parse() {
                    Ok(tempo) => {
                        data.tempo = tempo;
                        Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::TempoSet)))
                    }
                    Err(e) => Err(CmdError::ParseTempo(e)),
                }
            } else {
                Err(CmdError::CheckUsage(Cmd::Tempo))
            }
        },

        #[template = ""]
        #[help = "Print the current tempo of the route generator or pass it as an argument to the next command in the pipeline."]
        #[rets(TempoPrint = |cout, data, _args| console_log!(cout, Info, "current tempo: {}", data.tempo))]
        Print = |_cout, _cin, _data, args| {
            if args.is_empty() {
                Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::TempoPrint)))
            } else {
                Err(CmdError::CheckUsage(Cmd::Tempo))
            }
        },
    },
    #[input = "skip"]
    #[help = "Repeat all but the first n arguments."]
    Skip {
        #[template = "<N> [ANY]..."]
        #[help = "Pass all arguments after the first `n` arguments (after the argument for `n`) as arguments to the next command in the pipeline."]
        #[rets()]
        Basic = |_cout, _cin, _data, args| {
            if let [n_arg, rest @ ..] = args && n_arg.chars().all(char::is_numeric) {
                let n = n_arg.parse::<usize>().map_err(CmdError::ParseInt)?;
                Ok(CmdPromise::Ready(CmdReturn::pure(rest.iter().skip(n).map(<&str>::to_string).collect())))
            } else {
                Err(CmdError::CheckUsage(Cmd::Skip))
            }
        },
    },
    #[input = "take"]
    #[help = "Only repeat the first n arguments."]
    Take {
        #[template = "<N> [ANY]..."]
        #[help = "Pass the first `n` arguments (after the argument for `n`) as arguments to the next command in the pipeline."]
        #[rets()]
        Basic = |_cout, _cin, _data, args| {
            if let [n_arg, rest @ ..] = args && n_arg.chars().all(char::is_numeric) {
                let n = n_arg.parse::<usize>().map_err(CmdError::ParseInt)?;
                Ok(CmdPromise::Ready(CmdReturn::pure(rest.iter().take(n).map(<&str>::to_string).collect())))
            } else {
                Err(CmdError::CheckUsage(Cmd::Take))
            }
        },
    },
}

trait ParseVert {
    type Err;

    fn parse_vert(&self, graph: &WeightedGraph) -> Result<VertexID, Self::Err>;
}

impl ParseVert for str {
    type Err = CmdError;

    fn parse_vert(&self, graph: &WeightedGraph) -> Result<VertexID, Self::Err> {
        graph.find_vert(self)
            .ok_or_else(|| CmdError::VertexDNE(self.to_string()))
    }
}

pub enum CmdPromise {
    Ready(CmdReturn),
    InteractiveTargets,
    Route,
}

impl CmdPromise {
    pub fn poll(&mut self, cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData) -> Poll<Result<CmdReturn, CmdError>> {
        match self {
            Self::Ready(x) => Poll::Ready(Ok(std::mem::take(x))),
            Self::InteractiveTargets => {
                if data.is_giving_interactive_targets {
                    Poll::Pending
                } else {
                    let args = std::mem::take(&mut data.interactive_targets).into_iter()
                        .map(|target| data.graph.vert(target).id.clone())
                        .collect::<Vec<String>>();

                    let args = args.iter()
                        .map(String::as_str)
                        .collect::<Vec<&str>>();

                    match (cmd::SvRoute::Immediate.runner())(cout, cin, data, &args) {
                        Ok(x) => {
                            *self = x;
                            self.poll(cout, cin, data)
                        }
                        Err(e) => Poll::Ready(Err(e)),
                    }
                }
            }
            Self::Route => {
                if let Some(route) = data.route.as_ref() {
                    if route.is_finished() {
                        let results = route.result().iter()
                            .map(|&v| data.graph.vert(v).id.clone())
                            .collect::<Vec<String>>();

                        Poll::Ready(Ok(CmdReturn::new(results, CmdRetDisplay::SvRouteImmediate)))
                    } else {
                        Poll::Pending
                    }
                } else {
                    Poll::Ready(Err(CmdError::NoExistingRoute))
                }
            }
        }
    }
}

pub struct Routine {
    prev_ret: CmdPromise,
    src: Vec<String>,
}

impl Routine {
    pub fn new(src: &str) -> Self {
        Self {
            prev_ret: CmdPromise::Ready(CmdReturn::void()),
            src: src
                .split('|')
                .map(|s| s.trim().to_string())
                .rev()
                .collect(),
        }
    }

    pub fn step(&mut self, cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData) -> ControlFlow<Result<CmdReturn, CmdError>> {
        match self.prev_ret.poll(cout, cin, data) {
            Poll::Ready(Ok(prev_ret)) => {
                if let Some(line) = self.src.pop() {
                    let item = &line
                        .split_whitespace()
                        .chain(prev_ret.rets.iter().map(String::as_str))
                        .collect::<Vec<&str>>()
                        [..];

                    if let [cmd, args @ ..] = item {
                        let result = cmd.parse::<Cmd>()
                            .and_then(|cmd| cmd.run(cout, cin, data, args));

                        match result {
                            Ok(ret) => {
                                self.prev_ret = ret;
                                ControlFlow::Continue(())
                            }
                            Err(e) => ControlFlow::Break(Err(e)),
                        }
                    } else {
                        ControlFlow::Break(Err(CmdError::NoSuchCmd(String::new())))
                    }
                } else {
                    ControlFlow::Break(Ok(prev_ret))
                }
            }
            Poll::Ready(Err(e)) => ControlFlow::Break(Err(e)),
            Poll::Pending => ControlFlow::Continue(()),
        }
    }
}

fn help_all_msg() -> String {
    let cmd_column_width = Cmd::LIST.into_iter()
        .map(|cmd| cmd.input().len())
        .max()
        .unwrap() + 2;

    let mut msg = String::with_capacity(2048);
    msg.push_str("<color = #48f19d>Commands:</color>");
    for cmd in &Cmd::LIST {
        msg.push_str("\n    <color = #0096e6>");
        let input = cmd.input();
        msg.push_str(cmd.input());
        msg.push_str("</color>");
        for _ in input.len()..cmd_column_width {
            msg.push(' ');
        }
        msg.push_str(cmd.help());
    }
    msg
}

impl Cmd {
    fn help_msg(self) -> String {
        let template_column_width = self.usage_list().iter()
            .map(|usage| self.input().len() + if !usage.template().is_empty() { usage.template().len() + 1 } else { 0 })
            .max()
            .unwrap() + 2;

        let mut msg = String::with_capacity(2048);
        msg.push_str("<color = #48f19d>Usage:</color>");
        for usage in self.usage_list() {
            msg.push_str("\n    <color = #0096e6>");
            let input = self.input();
            msg.push_str(input);
            msg.push_str("</color>");
            let template = usage.template();
            let column_used = if !template.is_empty() {
                msg.push_str(" <color = #0096e6aa>");
                msg.push_str(template);
                msg.push_str("</color>");
                input.len() + template.len() + 1
            } else {
                input.len()
            };
            let mut lines_it = usage.help().lines();
            if let Some(line) = lines_it.next() {
                for _ in column_used..template_column_width {
                    msg.push(' ');
                }
                msg.push_str(line);

                for line in lines_it {
                    msg.push_str("\n    ");
                    for _ in 0..template_column_width {
                        msg.push(' ');
                    }
                    msg.push_str(line);
                }
            }
        }
        msg
    }
}

macro_rules! reserved_names {
    (
        $(#[$m:meta])*
        $vis:vis enum $Enum:ident {
            $(
                #[reason = $reason:expr]
                $(#[$vm:meta])*
                $Variant:ident = $name:literal
            ),* $(,)?
        }
    ) => {
        $(#[$m])*
        $vis enum $Enum {
            $(
                $(#[$vm])*
                $Variant,
            )*
        }

        impl $Enum {
            pub fn find(name: &str) -> Option<Self> {
                match name {
                    $($name => Some(Self::$Variant),)*
                    _ => None,
                }
            }

            pub const fn reason(&self) -> &'static str {
                match self {
                    $(Self::$Variant => $reason,)*
                }
            }
        }

        impl std::fmt::Display for $Enum {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self { $(Self::$Variant => $name,)* }.fmt(f)
            }
        }
    };
}

reserved_names!{
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum ReservedName {
        #[reason = "used by `sv.route` for generating routes interactively"]
        Interactive = "interactive",

        #[reason = "may be mistaken for a \'help\' request on another command"]
        Help = "help",
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum CmdError {
    /// Not yet implemented
    Todo,
    CheckUsage(Cmd),
    VertexDNE(String),
    DuplicateVertexName(String),
    BadVertexName(ReservedName),
    NoExistingRoute,
    EdgeLoop,
    ParseCoords(ParseCoordsError),
    ParseColor(ParseColorError),
    ParseBool(std::str::ParseBoolError),
    ParseInt(std::num::ParseIntError),
    ParseFloat(std::num::ParseFloatError),
    ParseTempo(ParseTempoError),
    LoadGraph(LoadGraphError),
    NoSuchCmd(String),
    IOError(std::io::Error),
}

impl std::fmt::Display for CmdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Todo => f.write_str("not yet implemented"),
            Self::CheckUsage(cmd) => f.write_str(cmd.help_msg().as_str()),
            Self::VertexDNE(id) => write!(f, "vertex \"{id}\" does not exist"),
            Self::BadVertexName(name) => write!(f, "vertices cannot be named \"{name}\". reason: {}", name.reason()),
            Self::DuplicateVertexName(name) => write!(f, "the name \"{name}\" is already in use by an existing vertex"),
            Self::NoExistingRoute => f.write_str("no route currently exists"),
            Self::EdgeLoop => f.write_str("vertices cannot have duplicate edges or edges to themselves"),
            Self::ParseCoords(_) => f.write_str("could not parse coordinates"),
            Self::ParseColor(_) => f.write_str("could not parse color"),
            Self::ParseBool(_) => f.write_str("could not parse boolean"),
            Self::ParseInt(_) => f.write_str("could not parse integer"),
            Self::ParseFloat(_) => f.write_str("could not parse float"),
            Self::ParseTempo(_) => f.write_str("could not parse tempo"),
            Self::LoadGraph(_) => f.write_str("could not load graph"),
            Self::NoSuchCmd(cmd) => write!(f, "no such command `{cmd}`"),
            Self::IOError(_) => f.write_str("filesystem IO error"),
        }
    }
}

impl std::error::Error for CmdError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            | Self::Todo
            | Self::CheckUsage(_)
            | Self::VertexDNE(_)
            | Self::BadVertexName(_)
            | Self::DuplicateVertexName(_)
            | Self::NoExistingRoute
            | Self::EdgeLoop
            | Self::NoSuchCmd(_)
                => None,

            Self::ParseCoords(e) => Some(e),
            Self::ParseColor(e) => Some(e),
            Self::ParseBool(e) => Some(e),
            Self::ParseInt(e) => Some(e),
            Self::ParseFloat(e) => Some(e),
            Self::ParseTempo(e) => Some(e),
            Self::LoadGraph(e) => Some(e),
            Self::IOError(e) => Some(e),
        }
    }
}

impl From<std::io::Error> for CmdError {
    fn from(value: std::io::Error) -> Self {
        CmdError::IOError(value)
    }
}
