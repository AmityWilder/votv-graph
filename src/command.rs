use std::{ops::ControlFlow, str::FromStr, task::Poll};
use raylib::prelude::*;
// use snippet::Snippet;
use crate::{console::{input::ConsoleIn, output::ConsoleOut}, console_log, graph::{VertexID, WeightedGraph}, route::RouteGenerator, serialization::LoadGraphError, types::{ParseColorError, ParseCoordsError, ParseTempoError, RichColor, Tempo}, CAMERA_POSITION_DEFAULT, VERTEX_RADIUS};

pub mod snippet;
// pub mod exec;
// mod cmd;

/// All information that can be affected by commands
pub struct ProgramData {
    pub graph: WeightedGraph,
    pub route: Option<RouteGenerator>,
    pub is_debugging: bool,
    pub camera: Camera3D,
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
                    $(#[$usage_meta:meta])*
                    $Usage:ident($def:expr)
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

            pub fn run(self, cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
                match self {
                    $(
                        Self::$Command => {
                            $(
                                let x: Result<CmdPromise, CmdError> = ($def)(cout, cin, data, args);
                                if !matches!(x, Err(CmdError::CheckUsage(_))) {
                                    return x;
                                }
                            )+
                            Err(CmdError::CheckUsage(self))
                        }
                    ),+
                }
            }
        }

        mod cmd {
            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                pub enum $Command {
                    $($Usage),+
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
    };
}

define_commands!{
    #[input = "help"]
    #[help = "Display information about commands."]
    Help {
        #[template = ""]
        #[help = "Display general information about all commands."]
        All(run_help_all),
        #[template = "<COMMAND>"]
        #[help = "Display more specific information about a specific command."]
        One(run_help_one),
    },
    #[input = "close"]
    #[help = "Close the application."]
    Close {
        #[template = ""]
        #[help = "Close the application immediately."]
        Basic(run_close),
    },
    #[input = "cls"]
    #[help = "Clear the console."]
    Cls {
        #[template = ""]
        #[help = "Clear the console history text."]
        Basic(run_cls),
    },
    #[input = "echo"]
    #[help = "Repeat the arguments."]
    Echo {
        #[template = "[ANY]..."]
        #[help = "Print the argument(s) or pass them as arguments to the next command in the pipeline."]
        Basic(run_echo),
    },
    #[input = "dbg"]
    #[help = "Toggle debug messages."]
    Dbg {
        #[template = ""]
        #[help = "Toggle whether debug messages are displayed."]
        Toggle(run_dbg_toggle),
        #[template = "true|false"]
        #[help = "Enable or disable the display of debug messages."]
        Set(run_dbg_set),
    },
    #[input = "focus"]
    #[help = "Focus a vertex."]
    Focus {
        #[template = "<ID|ALIAS>"]
        #[help = "Pan and zoom the camera to focus the specified vertex."]
        Vertex(run_focus_vertex),
        #[template = "reset"]
        #[help = "Reset the pan and zoom to the original orientation."]
        Reset(run_focus_reset),
        #[template = ""]
        #[help = "Print the ID of the currently focused vertex or pass it as an argument to the next command in the pipeline."]
        Print(run_focus_print),
    },
    #[input = "color.verts"]
    #[help = "Set the color of all vertices."]
    ColorVerts {
        #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
        #[help = "Set the base color used to render all vertices."]
        General(run_color_verts_general),
        #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME> <ID|ALIAS>..."]
        #[help = "[UNDER CONSTRUCTION] Set the base color used to render the specified vertices."]
        Specific(run_color_verts_specific),
    },
    #[input = "color.edges"]
    #[help = "Set the color of all edges."]
    ColorEdges {
        #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
        #[help = "Set the base color used to render all edges."]
        Basic(run_color_edges),
    },
    #[input = "color.bg"]
    #[help = "Set the color of the background."]
    ColorBackground {
        #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
        #[help = "Set the base color used to render background."]
        Basic(run_color_background),
    },
    #[input = "sv.route"]
    #[help = "Generate a route."]
    SvRoute {
        #[template = "<ID|ALIAS> <ID|ALIAS>..."]
        #[help = "Begin generating a route starting from the first vertex and visiting every following vertex in an order the minimizes total distance travelled."]
        Immediate(run_sv_route_immediate),
        #[template = "-i|interactive"]
        #[help = "Interactively provide targets to generate a route with by clicking on vertices in the visualizer with your mouse. \
                  Run the command `sv.route` without arguments when you are finished selecting targets."]
        Interactive(run_sv_route_interactive),
    },
    #[input = "sv.route.add"]
    #[help = "Add targets to the current route."]
    SvRouteAdd {
        #[template = ""]
        #[help = "Add more targets for the current route to visit. If any have already been visited, the generator will target them again \
                  as if the current final result is the start of a new route."]
        Immediate(run_sv_route_add_immediate),
        #[template = "-i|interactive"]
        #[help = "Interactively provide targets to generate a route with by clicking on vertices in the visualizer with your mouse. \
                  Run the command `sv.route` without arguments when you are finished selecting targets."]
        Interactive(run_sv_route_add_interactive),
    },
    #[input = "sv.route.list"]
    #[help = "List the results of the current route."]
    SvRouteList {
        #[template = ""]
        #[help = "Print the results of the current route in order or pass them as arguments to the next command in the pipeline."]
        Basic(run_sv_route_list),
    },
    #[input = "sv.route.clear"]
    #[help = "Clear the current route."]
    SvRouteClear {
        #[template = ""]
        #[help = "Remove all targets, results, and visuals from the current route."]
        Basic(run_sv_route_clear),
    },
    #[input = "sv.new"]
    #[help = "Create a new vertex."]
    SvNew {
        #[template = "<ID> [ALIAS] x:<???>/y:<???>[/z:<???>]"]
        #[help = "Create a new vertex with the specified name/alias at the specified position. It will start with no edges. The alias can be the same as the ID, \
                  but neither the ID nor alias may match the ID of any existing vertex, nor may either match the alias of any existing vertex."]
        Basic(run_sv_new),
    },
    #[input = "sv.edge"]
    #[help = "Create a new edge."]
    SvEdge {
        #[template = "<ID|ALIAS> [--] <ID|ALIAS>..."]
        #[help = "Create two-way edges connecting the prior vertex to each of the following vertices not separated by `--`s. A vertex cannot have an edge to itself. \
                  When one or more vertices not separated with `--`s are followed by a `--`, each vertex in that subset will be connected to each vertex in the following subset."]
        Basic(run_sv_edge),
    },
    #[input = "sv.load"]
    #[help = "Load a graph from a file."]
    SvLoad {
        #[template = "<PATH>"]
        #[help = "Load a weight graph map from the specified file path."]
        Basic(run_sv_load),
    },
    #[input = "sv.save"]
    #[help = "Save the graph to a file."]
    SvSave {
        #[template = "<PATH>"]
        #[help = "Save the current weighted graph map to the specified file path. If the file does not exist, it will be created."]
        Basic(run_sv_save),
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
        Basic(run_tempo),
    },
    #[input = "skip"]
    #[help = "Repeat all but the first n arguments."]
    Skip {
        #[template = "<N> [ANY]..."]
        #[help = "Pass all arguments after the first `n` arguments (after the argument for `n`) as arguments to the next command in the pipeline."]
        Basic(run_skip),
    },
    #[input = "take"]
    #[help = "Only repeat the first n arguments."]
    Take {
        #[template = "<N> [ANY]..."]
        #[help = "Pass the first `n` arguments (after the argument for `n`) as arguments to the next command in the pipeline."]
        Basic(run_take),
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

                    match run_sv_route_immediate(cout, cin, data, &args) {
                        Ok(mut x) => x.poll(cout, cin, data),
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

pub enum CmdRetDisplay {
    HelpAll,
    HelpOne(Cmd),
    Echo,
    Dbg,
    FocusVertex,
    FocusPrint,
    ColorVertsGeneral,
    ColorEdges,
    ColorBackground,
    SvRouteImmediate,
}

impl CmdRetDisplay {
    pub fn display(self, cout: &mut ConsoleOut, data: &ProgramData, args: Vec<String>) {
        match self {
            Self::HelpAll => console_log!(cout, Info, "{}", help_all_msg()),
            Self::HelpOne(cmd) => console_log!(cout, Info, "{}", cmd.help_msg()),
            Self::Echo => console_log!(cout, Info, "{}", args.join(" ")),
            Self::Dbg => console_log!(cout, Info, "debug messages are now {}", if data.is_debugging { "enabled" } else { "disabled" }),
            Self::FocusVertex => console_log!(cout, Info, "focused vertex {}", args[0]),
            Self::FocusPrint => if args.is_empty() {
                console_log!(cout, Info, "no vertex is currently focused");
            } else {
                console_log!(cout, Info, "currently focusing vertex {}", args[0]);
            },
            Self::ColorVertsGeneral => console_log!(cout, Info, "updated vertex color"),
            Self::ColorEdges => console_log!(cout, Info, "updated edge color"),
            Self::ColorBackground => console_log!(cout, Info, "updated background color"),
            Self::SvRouteImmediate => console_log!(cout, Route, "{}", args.join(" - ")),
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

fn run_help_all(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if args.is_empty() {
        Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::HelpAll)))
    } else {
        Err(CmdError::CheckUsage(Cmd::Help))
    }
}

impl Cmd {
    fn help_msg(self) -> String {
        let template_column_width = self.usage_list().into_iter()
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

fn run_help_one(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [cmd] = args {
        let cmd = cmd.parse::<Cmd>()
            .map_err(|_| CmdError::CheckUsage(Cmd::Help))?;

        Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::HelpOne(cmd))))
    } else {
        Err(CmdError::CheckUsage(Cmd::Help))
    }
}

fn run_close(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if args.is_empty() {
        data.should_close = true;
        Ok(CmdPromise::Ready(CmdReturn::void()))
    } else {
        Err(CmdError::CheckUsage(Cmd::Cls))
    }
}

fn run_cls(cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if args.is_empty() {
        cout.clear_log();
        Ok(CmdPromise::Ready(CmdReturn::void()))
    } else {
        Err(CmdError::CheckUsage(Cmd::Cls))
    }
}

fn run_echo(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Ok(CmdPromise::Ready(CmdReturn::new(args.into_iter().map(<&str>::to_string).collect(), CmdRetDisplay::Echo)))
}

fn run_dbg_toggle(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if args.is_empty() {
        data.is_debugging = !data.is_debugging;
        Ok(CmdPromise::Ready(CmdReturn::new(vec![if data.is_debugging { "true" } else { "false" }.to_string()], CmdRetDisplay::Dbg)))
    } else {
        Err(CmdError::CheckUsage(Cmd::Dbg))
    }
}

fn run_dbg_set(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [set_arg] = args {
        data.is_debugging = set_arg.parse::<bool>()
            .map_err(|e| CmdError::ParseBool(e))?;

        Ok(CmdPromise::Ready(CmdReturn::new(vec![if data.is_debugging { "true" } else { "false" }.to_string()], CmdRetDisplay::Dbg)))
    } else {
        Err(CmdError::CheckUsage(Cmd::Dbg))
    }
}

fn run_focus_vertex(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [target_arg] = args && &**target_arg != "reset" {
        let vert = data.graph.verts().iter()
            .find(|vert| (vert.id.eq_ignore_ascii_case(target_arg) || vert.alias.eq_ignore_ascii_case(target_arg)))
            .ok_or_else(|| CmdError::VertexDNE(target_arg.to_string()))?;

        data.camera.target = vert.pos;
        data.camera.position = data.camera.target + Vector3::new(0.0, 400.0, 0.0);
        Ok(CmdPromise::Ready(CmdReturn::new(vec![vert.alias.clone()], CmdRetDisplay::FocusVertex)))
    } else {
        Err(CmdError::CheckUsage(Cmd::Focus))
    }
}

fn run_focus_reset(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if matches!(args, ["reset"]) {
        data.camera.position = CAMERA_POSITION_DEFAULT;
        data.camera.target = Vector3::zero();
        Ok(CmdPromise::Ready(CmdReturn::void()))
    } else {
        Err(CmdError::CheckUsage(Cmd::Focus))
    }
}

fn run_focus_print(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if args.is_empty() {
        let vert = data.graph.verts().iter()
            .find(|vert| check_collision_spheres(vert.pos, VERTEX_RADIUS, data.camera.target, 1.0));

        Ok(CmdPromise::Ready(CmdReturn::new(vert.into_iter().map(|target| target.id.clone()).collect::<Vec<_>>(), CmdRetDisplay::FocusPrint)))
    } else {
        Err(CmdError::CheckUsage(Cmd::Focus))
    }
}

fn run_color_verts_general(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [color_arg] = args {
        data.verts_color = color_arg.parse::<RichColor>()
            .map(|RichColor(c)| c)
            .map_err(|e| CmdError::ParseColor(e))?;
        Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::ColorVertsGeneral)))
    } else {
        Err(CmdError::CheckUsage(Cmd::ColorVerts))
    }
}

fn run_color_verts_specific(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, _args: &[&str]) -> Result<CmdPromise, CmdError> {
    todo!("run_color_verts_specific requires per-vertex colors")
}

fn run_color_edges(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [color_arg] = args {
        data.edges_color = color_arg.parse::<RichColor>()
            .map(|RichColor(c)| c)
            .map_err(|e| CmdError::ParseColor(e))?;
        Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::ColorEdges)))
    } else {
        Err(CmdError::CheckUsage(Cmd::ColorVerts))
    }
}

fn run_color_background(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [color_arg] = args {
        data.background_color = color_arg.parse::<RichColor>()
            .map(|RichColor(c)| c)
            .map_err(|e| CmdError::ParseColor(e))?;
        Ok(CmdPromise::Ready(CmdReturn::disp(CmdRetDisplay::ColorBackground)))
    } else {
        Err(CmdError::CheckUsage(Cmd::ColorVerts))
    }
}

fn run_sv_route_immediate(cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [start, targets @ ..] = args && !targets.is_empty() && !matches!(&**start, "-i"|"interactive") {
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
}

fn run_sv_route_interactive(cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if matches!(args, ["-i"|"interactive"]) {
        console_log!(cout, Info,
            "click each target with the mouse; order doesn't matter except that the first will be the start \n\
             click a target again to un-target it\n\
             run the command `<color = #0096e6>sv.route</color>` (without arguments) when finished");
        cin.insert_over_selection("sv.route");
        data.is_giving_interactive_targets = true;
        Ok(CmdPromise::InteractiveTargets)
    } else if args.is_empty() && data.is_giving_interactive_targets {
        data.is_giving_interactive_targets = false;
        Ok(CmdPromise::Ready(CmdReturn::void()))
    } else {
        Err(CmdError::CheckUsage(Cmd::SvRoute))
    }
}

fn run_sv_route_add_immediate(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Err(CmdError::Todo)
}

fn run_sv_route_add_interactive(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if matches!(args, ["-i"|"interactive"]) {
        data.is_giving_interactive_targets = true;
        Ok(CmdPromise::InteractiveTargets)
    } else {
        Err(CmdError::CheckUsage(Cmd::SvRouteAdd))
    }
}

fn run_sv_route_list(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Err(CmdError::Todo)
}

fn run_sv_route_clear(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Err(CmdError::Todo)
}

fn run_sv_new(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Err(CmdError::Todo)
}

fn run_sv_edge(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Err(CmdError::Todo)
}

fn run_sv_load(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Err(CmdError::Todo)
}

fn run_sv_save(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Err(CmdError::Todo)
}

fn run_tempo(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    Err(CmdError::Todo)
}

fn run_skip(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [n_arg, rest @ ..] = args && n_arg.chars().all(char::is_numeric) {
        let n = n_arg.parse::<usize>().map_err(|e| CmdError::ParseInt(e))?;
        Ok(CmdPromise::Ready(CmdReturn::pure(rest.into_iter().skip(n).map(<&str>::to_string).collect())))
    } else {
        Err(CmdError::CheckUsage(Cmd::Skip))
    }
}

fn run_take(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<CmdPromise, CmdError> {
    if let [n_arg, rest @ ..] = args && n_arg.chars().all(char::is_numeric) {
        let n = n_arg.parse::<usize>().map_err(|e| CmdError::ParseInt(e))?;
        Ok(CmdPromise::Ready(CmdReturn::pure(rest.into_iter().take(n).map(<&str>::to_string).collect())))
    } else {
        Err(CmdError::CheckUsage(Cmd::Take))
    }
}


#[derive(Debug)]
pub enum CmdError {
    CheckUsage(Cmd),
    VertexDNE(String),
    NoExistingRoute,
    ParseCoords(ParseCoordsError),
    ParseColor(ParseColorError),
    ParseBool(std::str::ParseBoolError),
    ParseInt(std::num::ParseIntError),
    ParseFloat(std::num::ParseFloatError),
    ParseTempo(ParseTempoError),
    LoadGraph(LoadGraphError),
    NoSuchCmd(String),
    IOError(std::io::Error),
    Todo,
}

impl std::fmt::Display for CmdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::CheckUsage(cmd) => f.write_str(cmd.help_msg().as_str()),
            Self::VertexDNE(id) => write!(f, "vertex \"{id}\" does not exist"),
            Self::NoExistingRoute => f.write_str("no ongoing route to extend"),
            Self::ParseCoords(_) => f.write_str("could not parse coordinates"),
            Self::ParseColor(_) => f.write_str("could not parse color"),
            Self::ParseBool(_) => f.write_str("could not parse boolean"),
            Self::ParseInt(_) => f.write_str("could not parse integer"),
            Self::ParseFloat(_) => f.write_str("could not parse float"),
            Self::ParseTempo(_) => f.write_str("could not parse tempo"),
            Self::LoadGraph(_) => f.write_str("could not load graph"),
            Self::NoSuchCmd(cmd) => write!(f, "no such command `{cmd}`"),
            Self::IOError(_) => f.write_str("filesystem IO error"),
            Self::Todo => f.write_str("not yet implemented"),
        }
    }
}

impl std::error::Error for CmdError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            | Self::CheckUsage(_)
            | Self::VertexDNE(_)
            | Self::NoExistingRoute
            | Self::NoSuchCmd(_)
            | Self::Todo
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

// pub fn run(args: &str) -> Result<Cow<'_, str>, CmdError> {
//     let args.split_whitespace();
// }

// pub enum ArgValue<'a> {
//     Text(&'a str),
//     Literal(&'static str),
//     Command(Cmd),
//     Bool(bool),
//     Index(usize),
//     Int(i32),
//     Float(f32),
//     VertexID(VertexID),
//     Coords(Vector3),
//     Color(Color),
//     Array(Vec<ArgValue<'a>>),
// }

// pub struct Arg {
//     pub desc: &'static str,
//     pub disp: &'static str,
//     pub suggestions: for<'dat> fn(data: &'dat CommandData) -> Vec<Snippet<'dat>>,
//     pub validate: for<'a> fn(data: &'a CommandData, arg: ArgValue<'a>) -> Option<ArgValue<'a>>,
// }

// pub struct CmdUsage {
//     pub desc: &'static str,
//     pub args: &'static [Arg],
//     pub validate_args: for<'a> fn(data: &'a CommandData, arg: ArgValue<'a>) -> Option<ArgValue<'a>>,
//     pub run: fn(cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut CommandData, args: &[ArgValue]) -> CmdResult,
// }

// pub struct CmdInfo {
//     pub input: &'static str,
//     pub desc: &'static str,
//     pub usage: &'static [CmdUsage],
// }

// macro_rules! with_list {
//     (
//         $(#[$meta:meta])*
//         $vis:vis enum $Enum:ident {
//             $(
//                 $(#[$var_meta:meta])*
//                 $Variant:ident
//             ),+ $(,)?
//         }
//     ) => {
//         $(#[$meta])*
//         $vis enum $Enum {
//             $(
//                 $(#[$var_meta])*
//                 $Variant
//             ),+
//         }

//         impl $Enum {
//             pub const LIST: [Self; [$(Self::$Variant),+].len()] = [
//                 $(Self::$Variant),+
//             ];
//         }
//     };
// }

// with_list!{
//     #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//     pub enum Cmd {
//         Help,
//         Close,
//         Cls,
//         Echo,
//         Dbg,
//         Focus,
//         ColorVerts,
//         ColorEdges,
//         ColorBackground,
//         SvRoute,
//         SvRouteAdd,
//         SvRouteList,
//         SvRouteClear,
//         SvNew,
//         SvEdge,
//         SvLoad,
//         SvSave,
//         Tempo,
//         Skip,
//         Take,
//     }
// }

// impl Cmd {
//     pub fn execute(cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut CommandData) -> Result<(), CmdError> {
//         if let Some(line) = cin.submit_cmd(cout).map(|s| s.to_string()) {
//             let mut rets = Vec::new();
//             let mut args = Vec::new();
//             for item in line.split('|') {
//                 args.extend(item.split_whitespace().map(|x| ArgValue::Text(x)).chain(rets.into_iter().map(|(value, _)| value)));
//                 rets.clear();
//                 Self::run(cout, cin, data, args.into_iter(), &mut rets);
//                 args.clear();
//             }
//             for (x, p) in rets {
//                 p(cout, x)
//             }
//         }
//         Ok(())
//     }

//     fn run<'args>(
//         cout: &mut ConsoleOut,
//         cin: &mut ConsoleIn,
//         data: &mut CommandData,
//         args: impl Iterator<Item = ArgValue<'args>>,
//         rets: &mut Vec<(ArgValue<'_>, fn(&mut ConsoleOut, ArgValue<'_>))>,
//     ) -> Result<(), CmdError> {
//         if let Some(cmd) = args.next() {
//             let cmd = match cmd {
//                 ArgValue::Text(cmd) => cmd.parse::<Cmd>().map_err(|e| CmdError::FromCmdError(e))?,
//                 ArgValue::Command(cmd) => cmd,
//                 _ => todo!(),
//             };
//             let args = args.collect();
//             cmd.info().usage.iter()
//                 .map(|usage| usage.args.iter().zip(other))
//         }
//         Ok(())
//     }

//     pub const fn info(self) -> CmdInfo {
//         match self {
//             Self::Help => CmdInfo {
//                 input: "help",
//                 desc: "Display help information.",
//                 usage: &[
//                     CmdUsage {
//                         desc: "Display information about all commands.",
//                         args: &[],
//                         run: |_cout, _cin, _data, _args| todo!(),
//                     },
//                     CmdUsage {
//                         desc: "Display extra information about a specific command.",
//                         args: &[
//                             Arg {
//                                 desc: "The argument to display information for.",
//                                 disp: "<COMMAND>",
//                                 suggestions: suggest_any_command,
//                                 validate: valid_any_command,
//                             },
//                         ],
//                         run: |_cout, _cin, _data, _args| todo!(),
//                     },
//                 ],
//             },
//             Self::Close => CmdInfo {
//                 input: "close",
//                 desc: "Close the application.",
//                 usage: &[
//                     CmdUsage {
//                         desc: "Close the application.",
//                         args: &[],
//                         run: |_cout, _cin, data, _args| {
//                             data.should_close = true;
//                             Ok(String::new())
//                         },
//                     }
//                 ],
//             },
//             Self::Cls => CmdInfo {
//                 input: "cls",
//                 desc: "Clear the console.",
//                 usage: &[],
//             },
//             Self::Echo => CmdInfo {
//                 input: "echo",
//                 desc: "Repeat the input.",
//                 usage: &[
//                     CmdUsage {
//                         desc: "Print or return the arguments.",
//                         args: &[
//                             Arg {
//                                 desc: "The arguments to output",
//                                 disp: "[ANY]...",
//                                 suggestions: |_| Box::new(std::iter::empty()),
//                                 validate: |_, _| true,
//                             }
//                         ],
//                         run: todo!(),
//                     }
//                 ],
//             },
//             Self::Dbg => CmdInfo {
//                 input: "dbg",
//                 desc: "Toggle debug message display.",
//                 usage: &[
//                     CmdUsage {
//                         desc: "Print or return the arguments.",
//                         args: &[
//                             Arg {
//                                 desc: "The arguments to output",
//                                 disp: "[ANY]...",
//                                 suggestions: suggest_any_command,
//                                 validate: valid_any_command,
//                             }
//                         ],
//                     }
//                 ],
//             },
//             Self::Focus => CmdInfo {
//                 input: "focus",
//                 desc: "Focus a node.",
//                 usage: &[],
//             },
//             Self::ColorVerts => CmdInfo {
//                 input: "color.verts",
//                 desc: "Set the color of all vertices.",
//                 usage: &[],
//             },
//             Self::ColorEdges => CmdInfo {
//                 input: "color.edges",
//                 desc: "Set the color of all edges.",
//                 usage: &[],
//             },
//             Self::ColorBackground => CmdInfo {
//                 input: "color.bg",
//                 desc: "Set the color of the background.",
//                 usage: &[],
//             },
//             Self::SvRoute => CmdInfo {
//                 input: "sv.route",
//                 desc: "Generate a route.",
//                 usage: &[],
//             },
//             Self::SvRouteAdd => CmdInfo {
//                 input: "sv.route.add",
//                 desc: "Add targets to the current route.",
//                 usage: &[],
//             },
//             Self::SvRouteList => CmdInfo {
//                 input: "sv.route.list",
//                 desc: "List the results of the current route.",
//                 usage: &[],
//             },
//             Self::SvRouteClear => CmdInfo {
//                 input: "sv.route.clear",
//                 desc: "Clear the current route.",
//                 usage: &[],
//             },
//             Self::SvNew => CmdInfo {
//                 input: "sv.new",
//                 desc: "Create a new vertex.",
//                 usage: &[],
//             },
//             Self::SvEdge => CmdInfo {
//                 input: "sv.edge",
//                 desc: "Create a new edge.",
//                 usage: &[],
//             },
//             Self::SvLoad => CmdInfo {
//                 input: "sv.load",
//                 desc: "Load the graph from a file.",
//                 usage: &[],
//             },
//             Self::SvSave => CmdInfo {
//                 input: "sv.save",
//                 desc: "Save the graph to a file.",
//                 usage: &[],
//             },
//             Self::Tempo => CmdInfo {
//                 input: "tempo",
//                 desc: "Set the tickrate of the route generator.",
//                 usage: &[],
//             },
//             Self::Skip => CmdInfo {
//                 input: "skip",
//                 desc: "Ignore the first `n` arguments.",
//                 usage: &[],
//             },
//             Self::Take => CmdInfo {
//                 input: "take",
//                 desc: "Ignore all but the first `n` arguments.",
//                 usage: &[],
//             },
//         }
//     }
// }

// #[derive(Debug)]
// pub enum FromCmdError {
//     Unknown(String),
// }
// impl std::fmt::Display for FromCmdError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Unknown(cmd) => write!(f, "No such command `{cmd}`"),
//         }
//     }
// }
// impl std::error::Error for FromCmdError {}

// impl std::str::FromStr for Cmd {
//     type Err = FromCmdError;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         Self::LIST.into_iter()
//             .find(|cmd| s == cmd.info().input)
//             .ok_or_else(|| FromCmdError::Unknown(s.to_string()))
//     }
// }

// // macro_rules! define_commands {
// //     (
// //         $(#[$enum_meta:meta])*
// //         $vis:vis enum $Enum:ident {$(
// //             #[input($input:literal $(,)?)]
// //             #[usage($(case(args($($args:literal { $arg_validation:expr, $arg_predictions:expr }),* $(,)?), desc = $description:literal)),+ $(,)?)]
// //             $(#[$variant_meta:meta])*
// //             $Variant:ident
// //         ),* $(,)?}
// //     ) => {
// //         $(#[$enum_meta])*
// //         #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// //         $vis enum $Enum {$(
// //             $(#[$variant_meta])*
// //             #[doc = concat!(" # Command `", $input, "`\n ## Usage", $("\n - `", $input, $(" ", $args,)* "`: ", $description),+)]
// //             $Variant,
// //         )+}
// //         impl $Enum {
// //             pub const LIST: &[Self] = &[
// //                 $(Self::$Variant,)+
// //             ];
// //             pub fn try_from_str(s: &str) -> Result<Self, FromCmdError> {
// //                 match s {
// //                     $($input => Ok(Self::$Variant),)+
// //                     _ => Err(FromCmdError::Unknown(s.to_string())),
// //                 }
// //             }
// //             pub const fn input(&self) -> &'static str {
// //                 match self {
// //                     $(Self::$Variant => $input,)+
// //                 }
// //             }
// //             pub const fn args(&self) -> &'static [&'static str] {
// //                 match self {
// //                     $(Self::$Variant => &[$(concat!($(" ", $args),*),)*],)+
// //                 }
// //             }
// //             pub const fn usage(&self) -> &'static [&'static str] {
// //                 match self {
// //                     $(Self::$Variant => &[$(concat!($input, $(" ", $args),*),)*],)+
// //                 }
// //             }
// //             pub const fn description(&self) -> &'static [&'static str] {
// //                 match self {
// //                     $(Self::$Variant => &[$($description,)+],)+
// //                 }
// //             }
// //         }
// //     };
// // }

// // define_commands!{
// //     pub enum Cmd {
// //         #[input("help")]
// //         #[usage(
// //             case(args(), desc = "Display this information"),
// //             case(args(
// //                 "<COMMAND>" { todo!(), todo!() }
// //             ), desc = "Display help info about a particular command"),
// //         )]
// //         Help,

// //         #[input("close")]
// //         #[usage(case(args(), desc = "Close the application"))]
// //         Close,

// //         #[input("cls")]
// //         #[usage(case(args(), desc = "Clear the console"))]
// //         Cls,

// //         #[input("echo")]
// //         #[usage(case(args(), desc = "Print the input to the console"))]
// //         Echo,

// //         #[input("await")]
// //         #[usage(case(args(), desc = "Execute the arguments as an asynchronous command, blocking until complete"))]
// //         Await,

// //         #[input("dbg")]
// //         #[usage(case(args(), desc = "Toggle debug messages"))]
// //         Dbg,

// //         #[input("focus")]
// //         #[usage(
// //             case(args(
// //                 "<ID|ALIAS>" { todo!(), |data, s| data.graph.verts.contains(s) }
// //             ), desc = "Zoom in on a particular target"),
// //             case(args(
// //                 "reset" { todo!(), |_, s| s == "reset" }
// //             ), desc = "Reset camera orientation"),
// //             case(args(), desc = "Print the name of the focused vertex"),
// //         )]
// //         Focus,

// //         #[input("color.verts")]
// //         #[usage(
// //             case(args("#<HEXCODE>" { todo!(), todo!() }),                            desc = "Set the color of vertices with a hexcode"),
// //             case(args("rgb(<RED>, <GREEN>, <BLUE>)" { todo!(), todo!() }),           desc = "Set the color of vertices with RGB"),
// //             case(args("rgba(<RED>, <GREEN>, <BLUE>, <ALPHA>)" { todo!(), todo!() }), desc = "Set the color of vertices with RGB and transparency"),
// //             case(args("<NAME> [<OPACITY>%]" { todo!(), todo!() }),                   desc = "Set the color of vertices to a named color"),
// //         )]
// //         ColorVerts,

// //         #[input("color.edges")]
// //         #[usage(
// //             case(args("#<HEXCODE>" { todo!(), todo!() }),                            desc = "Set the color of edges with a hexcode"),
// //             case(args("rgb(<RED>, <GREEN>, <BLUE>)" { todo!(), todo!() }),           desc = "Set the color of edges with RGB"),
// //             case(args("rgba(<RED>, <GREEN>, <BLUE>, <ALPHA>)" { todo!(), todo!() }), desc = "Set the color of edges with RGB and transparency"),
// //             case(args("<NAME> [<OPACITY>%]" { todo!(), todo!() }),                   desc = "Set the color of edges to a named color"),
// //         )]
// //         ColorEdges,

// //         #[input("color.bg")]
// //         #[usage(
// //             case(args("#<HEXCODE>" { todo!(), todo!() }),                            desc = "Set the color of the background with a hexcode"),
// //             case(args("rgb(<RED>, <GREEN>, <BLUE>)" { todo!(), todo!() }),           desc = "Set the color of the background with RGB"),
// //             case(args("rgba(<RED>, <GREEN>, <BLUE>, <ALPHA>)" { todo!(), todo!() }), desc = "Set the color of the background with RGB and transparency"),
// //             case(args("<NAME> [<OPACITY>%]" { todo!(), todo!() }),                   desc = "Set the color of the background to a named color"),
// //         )]
// //         ColorBackground,

// //         #[input("sv.route")]
// //         #[usage(
// //             case(args("<START>" { todo!(), todo!() }, "<ID|ALIAS>..." { todo!(), todo!() }), desc = "Generate the shortest route visiting each target (separated by spaces)"),
// //             case(args("-i|interactive" { todo!(), todo!() }),           desc = "Provide targets to the route generator through the graphic interface"),
// //         )]
// //         SvRoute,

// //         #[input("sv.route.add")]
// //         #[usage(
// //             case(args("<ID|ALIAS>..." { todo!(), todo!() }),  desc = "Add more targets (separated by spaces) to the current route"),
// //             case(args("-i|interactive" { todo!(), todo!() }), desc = "Add more targets to the current route through the graphic interface"),
// //         )]
// //         SvRouteAdd,

// //         #[input("sv.route.list")]
// //         #[usage(case(args(), desc = "List the order of the targets in the current route"))]
// //         SvRouteList,

// //         #[input("sv.route.clear")]
// //         #[usage(case(args(), desc = "Clear the ongoing route"))]
// //         SvRouteClear,

// //         #[input("sv.new")]
// //         #[usage(
// //             case(args("<ID>" { todo!(), todo!() }, "[ALIAS]" { todo!(), todo!() }, "x:<???>/y:<???>" { todo!(), todo!() }), desc = "Create a new vertex that can be targeted at x,y"),
// //             case(args("<ID>" { todo!(), todo!() }, "[ALIAS]" { todo!(), todo!() }, "focus" { todo!(), todo!() }),           desc = "Create a new vertex that can be targeted at the focused position"),
// //         )]
// //         SvNew,

// //         #[input("sv.edge")]
// //         #[usage(
// //             case(args("<ID|ALIAS>" { todo!(), todo!() }, "<ID|ALIAS>" { todo!(), todo!() }), desc = "Create an edge connecting two existing vertices"),
// //             case(args("<ID|ALIAS>" { todo!(), todo!() }),               desc = "Print a list of all vertices adjacent to the target"),
// //         )]
// //         SvEdge,

// //         #[input("sv.load")]
// //         #[usage(case(args("<PATH>" { todo!(), todo!() }), desc = "Load another map from a graph file"))]
// //         SvLoad,

// //         #[input("sv.save")]
// //         #[usage(case(args("<PATH>" { todo!(), todo!() }), desc = "Save the current map to a graph file"))]
// //         SvSave,

// //         #[input("tempo")]
// //         #[usage(
// //             case(args("<TICKS>/<MILLISECONDS>" { todo!(), todo!() }), desc = "Set the route tick speed in ticks per milliseconds"),
// //             case(args("reset" { todo!(), todo!() }),                  desc = "Set the route tick speed to the default (1 step per millisecond)"),
// //             case(args("sync" { todo!(), todo!() }),                   desc = "Sync the route tick speed with the framerate"),
// //             case(args("sprint" { todo!(), todo!() }),                 desc = "Set the route tick speed to the maximum (1 step every 0 milliseconds)"),
// //             case(args(),                         desc = "Print the current tempo"),
// //         )]
// //         Tempo,

// //         #[input("skip")]
// //         #[usage(case(args("<COUNT> [ANY]..."), desc = "Skip the specified number of arguments and pass the rest as output"))]
// //         Skip,

// //         #[input("take")]
// //         #[usage(case(args("<COUNT> [ANY]..."), desc = "Return the specified number of arguments"))]
// //         Take,
// //     }
// // }

// // impl Cmd {
// //     pub fn predictions<'args>(args: impl IntoIterator<Item = &'args str>) -> impl Iterator<Item = &'static str> + Clone {
// //         for item in args {

// //         }
// //     }
// // }
