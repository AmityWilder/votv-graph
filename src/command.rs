use std::{borrow::Cow, collections::VecDeque, ops::ControlFlow, str::FromStr, task::Poll};
use raylib::prelude::*;
// use snippet::Snippet;
use crate::{camera::Orbiter, console::{input::ConsoleIn, output::ConsoleOut}, console_log, graph::{VertexID, WeightedGraph}, route::RouteGenerator, serialization::LoadGraphError, types::{ParseColorError, ParseCoordsError, ParseTempoError, RichColor, Tempo}, CAMERA_LENGTH_DEFAULT, VERTEX_RADIUS};

pub mod snippet;
// pub mod exec;
// mod cmd;

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

enum RoutineActivity {
    Ongoing(CmdRunner),
    Finished(Option<CmdRet>),
}

pub struct Routine {
    prev: RoutineActivity,
    src: VecDeque<Cow<'static, str>>,
}

impl Routine {
    pub fn new(src: &str) -> Self {
        Self {
            prev: RoutineActivity::Finished(None),
            src: src
                .split('|')
                .map(|s| Cow::Owned(s.trim().to_string()))
                .rev()
                .collect(),
        }
    }

    pub fn step(&mut self, cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData) -> ControlFlow<Result<CmdRet, CmdError>> {
        loop {
            let prev = match &mut self.prev {
                RoutineActivity::Ongoing(run) => {
                    match run.step(cout, cin, data) {
                        Poll::Ready(Ok(ret)) => Some(ret),
                        Poll::Ready(Err(e)) => break ControlFlow::Break(Err(e)),
                        Poll::Pending => break ControlFlow::Continue(()),
                    }
                }
                RoutineActivity::Finished(ret) => ret.take(),
            };

            if let Some(line) = self.src.pop_front() {
                let args = &line
                    .split_whitespace()
                    .map(|s| Cow::Owned(s.to_string()))
                    .chain(prev.into_iter().flat_map(|prev| CmdRunner::into_args(prev, data).into_iter()))
                    .collect::<Vec<_>>()
                    [..];

                match CmdRunner::init(data, args) {
                    Ok(runner) => {
                        self.prev = RoutineActivity::Ongoing(runner);
                    }
                    Err(e) => break ControlFlow::Break(Err(e)),
                }
            } else {
                break ControlFlow::Break(Ok(prev.expect("should have at least one command")));
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Cmd {
    Help,
    Close,
    Cls,
    Echo,
    Dbg,
    Focus,
    Color,
    SvRoute,
    SvRouteAdd,
    SvRouteList,
    SvRouteClear,
    SvNew,
    SvEdge,
    SvLoad,
    SvSave,
    Tempo,
    Skip,
    Take,
}

impl FromStr for Cmd {
    type Err = CmdError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "help"           => Ok(Cmd::Help),
            "close"          => Ok(Cmd::Close),
            "cls"            => Ok(Cmd::Cls),
            "echo"           => Ok(Cmd::Echo),
            "dbg"            => Ok(Cmd::Dbg),
            "focus"          => Ok(Cmd::Focus),
            "color"          => Ok(Cmd::Color),
            "sv.route"       => Ok(Cmd::SvRoute),
            "sv.route.add"   => Ok(Cmd::SvRouteAdd),
            "sv.route.list"  => Ok(Cmd::SvRouteList),
            "sv.route.clear" => Ok(Cmd::SvRouteClear),
            "sv.new"         => Ok(Cmd::SvNew),
            "sv.edge"        => Ok(Cmd::SvEdge),
            "sv.load"        => Ok(Cmd::SvLoad),
            "sv.save"        => Ok(Cmd::SvSave),
            "tempo"          => Ok(Cmd::Tempo),
            "skip"           => Ok(Cmd::Skip),
            "take"           => Ok(Cmd::Take),
            _ => Err(CmdError::NoSuchCmd(s.to_string())),
        }
    }
}

struct UsageInfo {
    template: &'static str,
    desc: &'static str,
}

struct BasicInfo {
    input: &'static str,
    desc: &'static str,
    usage: &'static [UsageInfo],
}

impl Cmd {
    pub const fn info(self) -> &'static BasicInfo {
        match self {
            Cmd::Help => &BasicInfo {
                input: "help",
                desc: "Display information about commands.",
                usage: &[
                    UsageInfo {
                        template: "",
                        desc: "Display general information about all commands.",
                    },
                    UsageInfo {
                        template: "<COMMAND>",
                        desc: "Display more specific information about a specific command.",
                    },
                ],
            },
            Cmd::Close => &BasicInfo {
                input: "close",
                desc: "Close the application.",
                usage: &[
                    UsageInfo {
                        template: "",
                        desc: "Close the application immediately.",
                    },
                ],
            },
            Cmd::Cls => &BasicInfo {
                input: "cls",
                desc: "Clear the console.",
                usage: &[
                    UsageInfo {
                        template: "",
                        desc: "Clear the console history text.",
                    },
                ],
            },
            Cmd::Echo => &BasicInfo {
                input: "echo",
                desc: "Repeat the arguments.",
                usage: &[
                    UsageInfo {
                        template: "[ANY]...",
                        desc: "Print the argument(s) or pass them as arguments to the next command in the pipeline."
                    },
                ],
            },
            Cmd::Dbg => &BasicInfo {
                input: "dbg",
                desc: "Toggle debug messages.",
                usage: &[
                    UsageInfo {
                        template: "",
                        desc: "Toggle whether debug messages are displayed.",
                    },
                    UsageInfo {
                        template: "true|false",
                        desc: "Enable or disable the display of debug messages.",
                    },
                ],
            },
            Cmd::Focus => &BasicInfo {
                input: "focus",
                desc: "Focus a vertex.",
                usage: &[
                    UsageInfo {
                        template: "<ID|ALIAS>",
                        desc: "Pan and zoom the camera to focus the specified vertex.",
                    },
                    UsageInfo {
                        template: "reset",
                        desc: "Reset the pan and zoom to the original orientation.",
                    },
                    UsageInfo {
                        template: "",
                        desc: "Print the ID of the currently focused vertex or pass it as an argument to the next command in the pipeline.",
                    },
                ],
            },
            Cmd::Color => &BasicInfo {
                input: "color",
                desc: "Set visualizer colors.",
                usage: &[
                    UsageInfo {
                        template: "verts #<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>",
                        desc: "Set the base color used to render all vertices.",
                    },
                    UsageInfo {
                        template: "edges #<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>",
                        desc: "Set the base color used to render all edges.",
                    },
                    UsageInfo {
                        template: "bg #<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>",
                        desc: "Set the base color used to render background.",
                    },
                ],
            },
            Cmd::SvRoute => &BasicInfo {
                input: "sv.route",
                desc: "Generate a route.",
                usage: &[
                    UsageInfo {
                        template: "<ID|ALIAS> <ID|ALIAS>...",
                        desc: "Begin generating a route starting from the first vertex and visiting every following vertex in an order the minimizes total distance travelled.",
                    },
                    UsageInfo {
                        template: "-i|interactive",
                        desc: "Interactively provide targets to generate a route with by clicking on vertices in the visualizer with your mouse. \
                               Run the command `sv.route` without arguments when you are finished selecting targets.",
                    },
                ],
            },
            Cmd::SvRouteAdd => &BasicInfo {
                input: "sv.route.add",
                desc: "Add targets to the current route.",
                usage: &[
                    UsageInfo {
                        template: "",
                        desc: "Add more targets for the current route to visit. If any have already been visited, the generator will target them again \
                               as if the current final result is the start of a new route.",
                    },
                    UsageInfo {
                        template: "-i|interactive",
                        desc: "Interactively provide targets to generate a route with by clicking on vertices in the visualizer with your mouse. \
                               Run the command `sv.route` without arguments when you are finished selecting targets.",
                    },
                ],
            },
            Cmd::SvRouteList => &BasicInfo {
                input: "sv.route.list",
                desc: "List the results of the current route.",
                usage: &[
                    UsageInfo {
                        template: "",
                        desc: "Print the results of the current route in order or pass them as arguments to the next command in the pipeline.",
                    },
                ],
            },
            Cmd::SvRouteClear => &BasicInfo {
                input: "sv.route.clear",
                desc: "Clear the current route.",
                usage: &[
                    UsageInfo {
                        template: "",
                        desc: "Remove all targets, results, and visuals from the current route.",
                    },
                ],
            },
            Cmd::SvNew => &BasicInfo {
                input: "sv.new",
                desc: "Create a new vertex.",
                usage: &[
                    UsageInfo {
                        template: "<ID> [ALIAS] x:<???>/y:<???>[/z:<???>]",
                        desc: "Create a new vertex with the specified name/alias at the specified position. It will start with no edges. The alias can be the same as the ID, \
                               but neither the ID nor alias may match the ID of any existing vertex, nor may either match the alias of any existing vertex.",
                    },
                ],
            },
            Cmd::SvEdge => &BasicInfo {
                input: "sv.edge",
                desc: "Create a new edge.",
                usage: &[
                    UsageInfo {
                        template: "<ID|ALIAS> [--] <ID|ALIAS>...",
                        desc: "Create two-way edges connecting the prior vertex to each of the following vertices not separated by `--`s. A vertex cannot have an edge to itself. \
                               When one or more vertices not separated with `--`s are followed by a `--`, each vertex in that subset will be connected to each vertex in the following subset.",
                    },
                ],
            },
            Cmd::SvLoad => &BasicInfo {
                input: "sv.load",
                desc: "Load a graph from a file.",
                usage: &[
                    UsageInfo {
                        template: "<PATH>",
                        desc: "Load a weight graph map from the specified file path.",
                    },
                ],
            },
            Cmd::SvSave => &BasicInfo {
                input: "sv.save",
                desc: "Save the graph to a file.",
                usage: &[
                    UsageInfo {
                        template: "<PATH>",
                        desc: "Save the current weighted graph map to the specified file path. If the file does not exist, it will be created.",
                    },
                ],
            },
            Cmd::Tempo => &BasicInfo {
                input: "tempo",
                desc: "Set the tempo of the route generator.",
                usage: &[
                    UsageInfo {
                        template: "reset|sync|sprint|instant|pause|ticks:<???>/ms:<???>",
                        desc: "Set the rate at which the route generator performs update steps in ticks per millisecond. \
                               Ticks can only occur during frames, so if the 'ms' are lower the duration of a single frame, \
                               multiple ticks will occur during a frame and only the latest one will be visualized in that frame.\n\
                               - `reset`: set the tempo to 1 tick per millisecond.\n\
                               - `sync`: set the tempo to 1 tick per frame.\n\
                               - `sprint`: set the tempo to the fastest possible while keeping the current framerate.\n\
                               - `instant`: set the tempo to 1 tick per 0 milliseconds, blocking until the route is completed.\n\
                               - `pause`: set the tempo to 0 ticks per millisecond.",
                    },
                ],
            },
            Cmd::Skip => &BasicInfo {
                input: "skip",
                desc: "Repeat all but the first n arguments.",
                usage: &[
                    UsageInfo {
                        template: "<N> [ANY]...",
                        desc: "Pass all arguments after the first `n` arguments (after the argument for `n`) as arguments to the next command in the pipeline.",
                    },
                ],
            },
            Cmd::Take => &BasicInfo {
                input: "take",
                desc: "Only repeat the first n arguments.",
                usage: &[
                    UsageInfo {
                        template: "<N> [ANY]...",
                        desc: "Pass the first `n` arguments (after the argument for `n`) as arguments to the next command in the pipeline.",
                    },
                ],
            },
        }
    }
}

fn help_all_msg() -> String {
    const CMD_LIST: [Cmd; 18] = [
        Cmd::Help,
        Cmd::Close,
        Cmd::Cls,
        Cmd::Echo,
        Cmd::Dbg,
        Cmd::Focus,
        Cmd::Color,
        Cmd::SvRoute,
        Cmd::SvRouteAdd,
        Cmd::SvRouteList,
        Cmd::SvRouteClear,
        Cmd::SvNew,
        Cmd::SvEdge,
        Cmd::SvLoad,
        Cmd::SvSave,
        Cmd::Tempo,
        Cmd::Skip,
        Cmd::Take,
    ];

    let cmd_column_width = CMD_LIST.into_iter()
        .map(|cmd| cmd.info().input.len())
        .max()
        .unwrap() + 2;

    let mut msg = String::with_capacity(2048);
    msg.push_str("<color = #48f19d>Commands:</color>");
    for cmd in &CMD_LIST {
        msg.push_str("\n    <color = #0096e6>");
        let input = cmd.info().input;
        msg.push_str(cmd.info().input);
        msg.push_str("</color>");
        for _ in input.len()..cmd_column_width {
            msg.push(' ');
        }
        msg.push_str(cmd.info().desc);
    }
    msg
}

impl Cmd {
    fn help_msg(self) -> String {
        let template_column_width = self.info().usage.into_iter()
            .map(|usage| self.info().input.len() + if !usage.template.is_empty() { usage.template.len() + 1 } else { 0 })
            .max()
            .unwrap() + 2;

        let mut msg = String::with_capacity(2048);
        msg.push_str("<color = #48f19d>Usage:</color>");
        for usage in self.info().usage {
            msg.push_str("\n    <color = #0096e6>");
            let input = self.info().input;
            msg.push_str(input);
            msg.push_str("</color>");
            let template = usage.template;
            let column_used = if !template.is_empty() {
                msg.push_str(" <color = #0096e6aa>");
                msg.push_str(template);
                msg.push_str("</color>");
                input.len() + template.len() + 1
            } else {
                input.len()
            };
            let mut lines_it = usage.desc.lines();
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

pub trait Command: Sized {
    type Ret;

    fn init(data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError>;
    fn step(&mut self, cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>>;
    fn into_args(ret: Self::Ret, data: &ProgramData) -> Vec<Cow<'static, str>>;
    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, data: &ProgramData);
}

pub enum CmdRunner {
    Help        (CmdHelp),
    Close       (CmdClose),
    Cls         (CmdCls),
    Echo        (CmdEcho),
    Dbg         (CmdDbg),
    Focus       (CmdFocus),
    Color       (CmdColor),
    SvRoute     (CmdSvRoute),
    SvRouteAdd  (CmdSvRouteAdd),
    SvRouteList (CmdSvRouteList),
    SvRouteClear(CmdSvRouteClear),
    SvNew       (CmdSvNew),
    SvEdge      (CmdSvEdge),
    SvLoad      (CmdSvLoad),
    SvSave      (CmdSvSave),
    Tempo       (CmdTempo),
    Skip        (CmdSkip),
    Take        (CmdTake),
}

pub enum CmdRet {
    Help        (<CmdHelp            as Command>::Ret),
    Close       (<CmdClose           as Command>::Ret),
    Cls         (<CmdCls             as Command>::Ret),
    Echo        (<CmdEcho            as Command>::Ret),
    Dbg         (<CmdDbg             as Command>::Ret),
    Focus       (<CmdFocus           as Command>::Ret),
    Color       (<CmdColor           as Command>::Ret),
    SvRoute     (<CmdSvRoute         as Command>::Ret),
    SvRouteAdd  (<CmdSvRouteAdd      as Command>::Ret),
    SvRouteList (<CmdSvRouteList     as Command>::Ret),
    SvRouteClear(<CmdSvRouteClear    as Command>::Ret),
    SvNew       (<CmdSvNew           as Command>::Ret),
    SvEdge      (<CmdSvEdge          as Command>::Ret),
    SvLoad      (<CmdSvLoad          as Command>::Ret),
    SvSave      (<CmdSvSave          as Command>::Ret),
    Tempo       (<CmdTempo           as Command>::Ret),
    Skip        (<CmdSkip            as Command>::Ret),
    Take        (<CmdTake            as Command>::Ret),
}

impl Command for CmdRunner {
    type Ret = CmdRet;

    fn init(data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if let [cmd, args @ ..] = args {
            Ok(match cmd.parse::<Cmd>()? {
                Cmd::Help         => Self::Help        (CmdHelp        ::init(data, args)?),
                Cmd::Close        => Self::Close       (CmdClose       ::init(data, args)?),
                Cmd::Cls          => Self::Cls         (CmdCls         ::init(data, args)?),
                Cmd::Echo         => Self::Echo        (CmdEcho        ::init(data, args)?),
                Cmd::Dbg          => Self::Dbg         (CmdDbg         ::init(data, args)?),
                Cmd::Focus        => Self::Focus       (CmdFocus       ::init(data, args)?),
                Cmd::Color        => Self::Color       (CmdColor       ::init(data, args)?),
                Cmd::SvRoute      => Self::SvRoute     (CmdSvRoute     ::init(data, args)?),
                Cmd::SvRouteAdd   => Self::SvRouteAdd  (CmdSvRouteAdd  ::init(data, args)?),
                Cmd::SvRouteList  => Self::SvRouteList (CmdSvRouteList ::init(data, args)?),
                Cmd::SvRouteClear => Self::SvRouteClear(CmdSvRouteClear::init(data, args)?),
                Cmd::SvNew        => Self::SvNew       (CmdSvNew       ::init(data, args)?),
                Cmd::SvEdge       => Self::SvEdge      (CmdSvEdge      ::init(data, args)?),
                Cmd::SvLoad       => Self::SvLoad      (CmdSvLoad      ::init(data, args)?),
                Cmd::SvSave       => Self::SvSave      (CmdSvSave      ::init(data, args)?),
                Cmd::Tempo        => Self::Tempo       (CmdTempo       ::init(data, args)?),
                Cmd::Skip         => Self::Skip        (CmdSkip        ::init(data, args)?),
                Cmd::Take         => Self::Take        (CmdTake        ::init(data, args)?),
            })
        } else {
            Err(CmdError::NoSuchCmd(String::new()))
        }
    }

    fn step(&mut self, cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        match self {
            Self::Help        (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Help        (ret))),
            Self::Close       (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Close       (ret))),
            Self::Cls         (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Cls         (ret))),
            Self::Echo        (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Echo        (ret))),
            Self::Dbg         (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Dbg         (ret))),
            Self::Focus       (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Focus       (ret))),
            Self::Color       (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Color       (ret))),
            Self::SvRoute     (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::SvRoute     (ret))),
            Self::SvRouteAdd  (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::SvRouteAdd  (ret))),
            Self::SvRouteList (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::SvRouteList (ret))),
            Self::SvRouteClear(inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::SvRouteClear(ret))),
            Self::SvNew       (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::SvNew       (ret))),
            Self::SvEdge      (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::SvEdge      (ret))),
            Self::SvLoad      (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::SvLoad      (ret))),
            Self::SvSave      (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::SvSave      (ret))),
            Self::Tempo       (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Tempo       (ret))),
            Self::Skip        (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Skip        (ret))),
            Self::Take        (inner) => inner.step(cout, cin, data).map(|res| res.map(|ret| CmdRet::Take        (ret))),
        }
    }

    fn into_args(ret: Self::Ret, data: &ProgramData) -> Vec<Cow<'static, str>> {
        match ret {
            CmdRet::Help        (inner) => CmdHelp        ::into_args(inner, data),
            CmdRet::Close       (inner) => CmdClose       ::into_args(inner, data),
            CmdRet::Cls         (inner) => CmdCls         ::into_args(inner, data),
            CmdRet::Echo        (inner) => CmdEcho        ::into_args(inner, data),
            CmdRet::Dbg         (inner) => CmdDbg         ::into_args(inner, data),
            CmdRet::Focus       (inner) => CmdFocus       ::into_args(inner, data),
            CmdRet::Color       (inner) => CmdColor       ::into_args(inner, data),
            CmdRet::SvRoute     (inner) => CmdSvRoute     ::into_args(inner, data),
            CmdRet::SvRouteAdd  (inner) => CmdSvRouteAdd  ::into_args(inner, data),
            CmdRet::SvRouteList (inner) => CmdSvRouteList ::into_args(inner, data),
            CmdRet::SvRouteClear(inner) => CmdSvRouteClear::into_args(inner, data),
            CmdRet::SvNew       (inner) => CmdSvNew       ::into_args(inner, data),
            CmdRet::SvEdge      (inner) => CmdSvEdge      ::into_args(inner, data),
            CmdRet::SvLoad      (inner) => CmdSvLoad      ::into_args(inner, data),
            CmdRet::SvSave      (inner) => CmdSvSave      ::into_args(inner, data),
            CmdRet::Tempo       (inner) => CmdTempo       ::into_args(inner, data),
            CmdRet::Skip        (inner) => CmdSkip        ::into_args(inner, data),
            CmdRet::Take        (inner) => CmdTake        ::into_args(inner, data),
        }
    }

    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, data: &ProgramData) {
        match ret {
            CmdRet::Help        (inner) => CmdHelp        ::disp(inner, cout, data),
            CmdRet::Close       (inner) => CmdClose       ::disp(inner, cout, data),
            CmdRet::Cls         (inner) => CmdCls         ::disp(inner, cout, data),
            CmdRet::Echo        (inner) => CmdEcho        ::disp(inner, cout, data),
            CmdRet::Dbg         (inner) => CmdDbg         ::disp(inner, cout, data),
            CmdRet::Focus       (inner) => CmdFocus       ::disp(inner, cout, data),
            CmdRet::Color       (inner) => CmdColor       ::disp(inner, cout, data),
            CmdRet::SvRoute     (inner) => CmdSvRoute     ::disp(inner, cout, data),
            CmdRet::SvRouteAdd  (inner) => CmdSvRouteAdd  ::disp(inner, cout, data),
            CmdRet::SvRouteList (inner) => CmdSvRouteList ::disp(inner, cout, data),
            CmdRet::SvRouteClear(inner) => CmdSvRouteClear::disp(inner, cout, data),
            CmdRet::SvNew       (inner) => CmdSvNew       ::disp(inner, cout, data),
            CmdRet::SvEdge      (inner) => CmdSvEdge      ::disp(inner, cout, data),
            CmdRet::SvLoad      (inner) => CmdSvLoad      ::disp(inner, cout, data),
            CmdRet::SvSave      (inner) => CmdSvSave      ::disp(inner, cout, data),
            CmdRet::Tempo       (inner) => CmdTempo       ::disp(inner, cout, data),
            CmdRet::Skip        (inner) => CmdSkip        ::disp(inner, cout, data),
            CmdRet::Take        (inner) => CmdTake        ::disp(inner, cout, data),
        }
    }
}

enum CmdHelp {
    All,
    One(Cmd),
}

impl Command for CmdHelp {
    type Ret = String;

    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if args.is_empty() {
            Ok(Self::All)
        } else if let [cmd_str] = args {
            Ok(Self::One(cmd_str.parse::<Cmd>()?))
        } else {
            Err(CmdError::CheckUsage(Cmd::Help))
        }
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        Poll::Ready(Ok(match self {
            CmdHelp::All => help_all_msg(),
            CmdHelp::One(cmd) => cmd.help_msg(),
        }))
    }

    fn into_args(ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        vec![Cow::Owned(ret)]
    }

    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, _data: &ProgramData) {
        console_log!(cout, Info, "{ret}");
    }
}

struct CmdClose;

impl Command for CmdClose {
    type Ret = ();

    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if args.is_empty() {
            Ok(Self)
        } else {
            Err(CmdError::CheckUsage(Cmd::Close))
        }
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        data.should_close = true;
        Poll::Ready(Ok(()))
    }

    #[inline]
    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        Vec::new()
    }

    #[inline]
    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {}
}

struct CmdCls;

impl Command for CmdCls {
    type Ret = ();

    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if args.is_empty() {
            Ok(Self)
        } else {
            Err(CmdError::CheckUsage(Cmd::Close))
        }
    }

    fn step(&mut self, cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        cout.clear_log();
        Poll::Ready(Ok(()))
    }

    #[inline]
    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        Vec::new()
    }

    #[inline]
    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {}
}

#[derive(Default)]
struct CmdEcho(Vec<Cow<'static, str>>);

impl Command for CmdEcho {
    type Ret = Vec<Cow<'static, str>>;

    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        Ok(Self(args.to_owned()))
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        Poll::Ready(Ok(std::mem::take(&mut self.0)))
    }

    fn into_args(ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        ret
    }

    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, _data: &ProgramData) {
        console_log!(cout, Info, "{}", ret.join(" "))
    }
}

enum CmdDbg {
    Toggle,
    Set(bool),
}

impl Command for CmdDbg {
    type Ret = bool;

    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if args.is_empty() {
            Ok(Self::Toggle)
        } else if let [new_value_str] = args && let Ok(new_value) = new_value_str.parse::<bool>() {
            Ok(Self::Set(new_value))
        } else {
            Err(CmdError::CheckUsage(Cmd::Dbg))
        }
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        let new_value = match self {
            CmdDbg::Toggle => !data.is_debugging,
            CmdDbg::Set(value) => *value,
        };
        data.is_debugging = new_value;
        Poll::Ready(Ok(new_value))
    }

    fn into_args(ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        vec![Cow::Borrowed(if ret { "true" } else { "false" })]
    }

    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, _data: &ProgramData) {
        console_log!(cout, Info, "debug messages are now {}", if *ret { "enabled" } else { "disabled" })
    }
}

enum CmdFocus {
    Vertex(VertexID),
    Reset,
    Print,
}

enum CmdFocusRet {
    Vertex(VertexID),
    Reset,
    Print(Option<VertexID>),
}

impl Command for CmdFocus {
    type Ret = CmdFocusRet;

    fn init(data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if args.is_empty() {
            Ok(Self::Print)
        } else if let [name] = args {
            if name == "reset" {
                Ok(Self::Reset)
            } else if let Some(id) = data.graph.find_vert(name) {
                Ok(Self::Vertex(id))
            } else {
                Err(CmdError::VertexDNE(name.to_string()))
            }
        } else {
            Err(CmdError::CheckUsage(Cmd::Focus))
        }
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        match self {
            &mut CmdFocus::Vertex(id) => {
                let vert = data.graph.vert(id);
                data.orbit.target = vert.pos;
                data.orbit.length = 400.0;
                Poll::Ready(Ok(CmdFocusRet::Vertex(id)))
            }
            CmdFocus::Reset => {
                data.orbit.target = Vector3::zero();
                data.orbit.length = CAMERA_LENGTH_DEFAULT;
                Poll::Ready(Ok(CmdFocusRet::Reset))
            }
            CmdFocus::Print => {
                let id = data.graph.verts_iter()
                    .find_map(|(id, vert)| check_collision_spheres(vert.pos, VERTEX_RADIUS, data.orbit.target, 1.0).then_some(id));

                Poll::Ready(Ok(CmdFocusRet::Print(id)))
            }
        }

    }

    fn into_args(ret: Self::Ret, data: &ProgramData) -> Vec<Cow<'static, str>> {
        match ret {
            CmdFocusRet::Vertex(id) | CmdFocusRet::Print(Some(id)) => vec![Cow::Owned(data.graph.vert(id).id.clone())],
            CmdFocusRet::Print(None) => vec![Cow::Borrowed("")],
            CmdFocusRet::Reset => Vec::new(),
        }
    }

    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, data: &ProgramData) {
        match ret {
            &CmdFocusRet::Vertex(id) => console_log!(cout, Info, "now focusing vertex {}", data.graph.vert(id).alias),
            &CmdFocusRet::Print(Some(id)) => console_log!(cout, Info, "currently focusing vertex {}", data.graph.vert(id).alias),
            CmdFocusRet::Print(None) => console_log!(cout, Info, "no vertex is currently focused"),
            CmdFocusRet::Reset => console_log!(cout, Info, "reset focus to default"),
        }
    }
}

enum CmdColor {
    Verts(Color),
    Edges(Color),
    Background(Color),
}

enum CmdColorRet {
    Verts,
    Edges,
    Background,
}

impl Command for CmdColor {
    type Ret = CmdColorRet;

    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if let [region, color_str] = args {
            let color = color_str
                .parse::<RichColor>()
                .map(|RichColor(c)| c)
                .map_err(|e| CmdError::ParseColor(e));

            match region as &str {
                "verts" | "v" => Ok(Self::Verts(color?)),
                "edges" | "e" => Ok(Self::Edges(color?)),
                "background" | "bg" => Ok(Self::Background(color?)),
                _ => Err(CmdError::CheckUsage(Cmd::Color)),
            }
        } else {
            Err(CmdError::CheckUsage(Cmd::Color))
        }
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        Poll::Ready(Ok(match self {
            CmdColor::Verts(color) => {
                data.verts_color = *color;
                CmdColorRet::Verts
            },
            CmdColor::Edges(color) => {
                data.edges_color = *color;
                CmdColorRet::Edges
            },
            CmdColor::Background(color) => {
                data.background_color = *color;
                CmdColorRet::Background
            },
        }))
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        Vec::new()
    }

    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, _data: &ProgramData) {
        console_log!(cout, Info, "updated color for {}",
        match ret {
            CmdColorRet::Verts => "all vertices",
            CmdColorRet::Edges => "all edges",
            CmdColorRet::Background => "the background",
        })
    }
}

struct CmdSvRoute;

impl Command for CmdSvRoute {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdSvRouteAdd;

impl Command for CmdSvRouteAdd {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdSvRouteList;

impl Command for CmdSvRouteList {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdSvRouteClear;

impl Command for CmdSvRouteClear {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdSvNew;

impl Command for CmdSvNew {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdSvEdge;

impl Command for CmdSvEdge {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdSvLoad;

impl Command for CmdSvLoad {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdSvSave;

impl Command for CmdSvSave {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdTempo;

impl Command for CmdTempo {
    type Ret = ();

    fn init(_data: &ProgramData, _args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        todo!()
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        todo!()
    }

    fn into_args(_ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        todo!()
    }

    fn disp(_ret: &Self::Ret, _cout: &mut ConsoleOut, _data: &ProgramData) {
        todo!()
    }
}

struct CmdSkip(Vec<Cow<'static, str>>);

impl Command for CmdSkip {
    type Ret = Vec<Cow<'static, str>>;

    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if let [n_str, rest @ ..] = args {
            let n = n_str.parse::<usize>().map_err(|e| CmdError::ParseInt(e))?;
            Ok(Self(rest[n.min(rest.len())..].to_vec()))
        } else {
            Err(CmdError::CheckUsage(Cmd::Skip))
        }
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        Poll::Ready(Ok(std::mem::take(&mut self.0)))
    }

    fn into_args(ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        ret
    }

    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, _data: &ProgramData) {
        console_log!(cout, Info, "{}", ret.join(" "))
    }
}

struct CmdTake(Vec<Cow<'static, str>>);

impl Command for CmdTake {
    type Ret = Vec<Cow<'static, str>>;

    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> {
        if let [n_str, rest @ ..] = args {
            let n = n_str.parse::<usize>().map_err(|e| CmdError::ParseInt(e))?;
            Ok(Self(rest[..n.min(rest.len())].to_vec()))
        } else {
            Err(CmdError::CheckUsage(Cmd::Take))
        }
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Self::Ret, CmdError>> {
        Poll::Ready(Ok(std::mem::take(&mut self.0)))
    }

    fn into_args(ret: Self::Ret, _data: &ProgramData) -> Vec<Cow<'static, str>> {
        ret
    }

    fn disp(ret: &Self::Ret, cout: &mut ConsoleOut, _data: &ProgramData) {
        console_log!(cout, Info, "{}", ret.join(" "))
    }
}


// macro_rules! define_commands {
//     (
//         $(
//             #[input = $input:literal]
//             #[help = $help:literal]
//             $(#[$meta:meta])*
//             $Command:ident {
//                 $(
//                     #[template = $template:literal]
//                     #[help = $usage_help:literal]
//                     $(#[$usage_meta:meta])*
//                     $Usage:ident($CmdUsage:ty: { $Args:ty } -> { $Rets:ty })
//                 ),+ $(,)?
//             }
//         ),+ $(,)?
//     ) => {
//         #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//         pub enum Cmd {
//             $($Command),*
//         }

//         impl FromStr for Cmd {
//             type Err = CmdError;

//             fn from_str(s: &str) -> Result<Self, Self::Err> {
//                 match s {
//                     $($input => Ok(Self::$Command),)+
//                     _ => Err(CmdError::NoSuchCmd(s.to_string())),
//                 }
//             }
//         }

//         impl Cmd {
//             pub const LIST: [Self; [$(Self::$Command),+].len()] = [$(Self::$Command),+];

//             pub const fn usage_list(self) -> &'static [Usage] {
//                 match self {
//                     $(Self::$Command => &[$(Usage::$Command(cmd::$Command::$Usage)),+]),+
//                 }
//             }

//             pub const fn input(self) -> &'static str {
//                 match self {
//                     $(Self::$Command => $input),+
//                 }
//             }

//             pub const fn help(self) -> &'static str {
//                 match self {
//                     $(Self::$Command => $help),+
//                 }
//             }

//             pub fn run(self, cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData, args: &[Cow<'static, str>]) -> CmdResult {
//                 match self {
//                     $(
//                         Self::$Command => {
//                             $(if let Some(args) = <$Args as Arguments>::try_parse(args) {
//                                 return <$CmdUsage as Command>::run(cout, cin, data, args);
//                             } else)+ {
//                                 Err(CmdError::CheckUsage(self))
//                             }
//                         }
//                     ),+
//                 }
//             }
//         }

//         mod cmd {
//             $(
//                 $(#[$meta])*
//                 #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//                 pub enum $Command {
//                     $($Usage),+
//                 }
//             )+
//         }

//         #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//         pub enum Usage {
//             $($Command(cmd::$Command),)+
//         }

//         impl Usage {
//             pub const fn template(self) -> &'static str {
//                 match self {
//                     $($(Self::$Command(cmd::$Command::$Usage) => $template,)+)+
//                 }
//             }

//             pub const fn help(self) -> &'static str {
//                 match self {
//                     $($(Self::$Command(cmd::$Command::$Usage) => $usage_help,)+)+
//                 }
//             }
//         }
//     };
// }

// define_commands!{
//     #[input = "help"]
//     #[help = "Display information about commands."]
//     Help {
//         #[template = ""]
//         #[help = "Display general information about all commands."]
//         All(HelpAll: { () } -> { () }),

//         #[template = "<COMMAND>"]
//         #[help = "Display more specific information about a specific command."]
//         One(HelpOne: { () } -> { () }),
//     },

//     #[input = "close"]
//     #[help = "Close the application."]
//     Close {
//         #[template = ""]
//         #[help = "Close the application immediately."]
//         Basic(CloseBasic: { () } -> { () }),
//     },

//     #[input = "cls"]
//     #[help = "Clear the console."]
//     Cls {
//         #[template = ""]
//         #[help = "Clear the console history text."]
//         Basic(ClsBasic: { () } -> { () }),
//     },

//     #[input = "echo"]
//     #[help = "Repeat the arguments."]
//     Echo {
//         #[template = "[ANY]..."]
//         #[help = "Print the argument(s) or pass them as arguments to the next command in the pipeline."]
//         Basic(EchoBasic: { () } -> { () }),
//     },

//     #[input = "dbg"]
//     #[help = "Toggle debug messages."]
//     Dbg {
//         #[template = ""]
//         #[help = "Toggle whether debug messages are displayed."]
//         Toggle(DbgToggle: { () } -> { () }),

//         #[template = "true|false"]
//         #[help = "Enable or disable the display of debug messages."]
//         Set(DbgSet: { () } -> { () }),
//     },

//     #[input = "focus"]
//     #[help = "Focus a vertex."]
//     Focus {
//         #[template = "<ID|ALIAS>"]
//         #[help = "Pan and zoom the camera to focus the specified vertex."]
//         Vertex(FocusVertex: { () } -> { () }),

//         #[template = "reset"]
//         #[help = "Reset the pan and zoom to the original orientation."]
//         Reset(FocusReset: { () } -> { () }),

//         #[template = ""]
//         #[help = "Print the ID of the currently focused vertex or pass it as an argument to the next command in the pipeline."]
//         Print(FocusPrint: { () } -> { () }),
//     },

//     #[input = "color.verts"]
//     #[help = "Set the color of all vertices."]
//     ColorVerts {
//         #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
//         #[help = "Set the base color used to render all vertices."]
//         General(ColorVertsGeneral: { () } -> { () }),

//         #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME> <ID|ALIAS>..."]
//         #[help = "[UNDER CONSTRUCTION] Set the base color used to render the specified vertices."]
//         Specific(ColorVertsSpecific: { () } -> { () }),
//     },

//     #[input = "color.edges"]
//     #[help = "Set the color of all edges."]
//     ColorEdges {
//         #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
//         #[help = "Set the base color used to render all edges."]
//         Basic(ColorEdgesBasic: { () } -> { () }),
//     },

//     #[input = "color.bg"]
//     #[help = "Set the color of the background."]
//     ColorBackground {
//         #[template = "#<HEXCODE>|rgb(<???>,<???>,<???>)|rgba(<???>,<???>,<???>,<???>)|<NAME>"]
//         #[help = "Set the base color used to render background."]
//         Basic(ColorBackgroundBasic: { () } -> { () }),
//     },

//     #[input = "sv.route"]
//     #[help = "Generate a route."]
//     SvRoute {
//         #[template = "<ID|ALIAS> <ID|ALIAS>..."]
//         #[help = "Begin generating a route starting from the first vertex and visiting every following vertex in an order the minimizes total distance travelled."]
//         Immediate(SvRouteImmediate: { () } -> { () }),

//         #[template = "-i|interactive"]
//         #[help = "Interactively provide targets to generate a route with by clicking on vertices in the visualizer with your mouse. \
//                   Run the command `sv.route` without arguments when you are finished selecting targets."]
//         Interactive(SvRouteInteractive: { () } -> { () }),
//     },

//     #[input = "sv.route.add"]
//     #[help = "Add targets to the current route."]
//     SvRouteAdd {
//         #[template = ""]
//         #[help = "Add more targets for the current route to visit. If any have already been visited, the generator will target them again \
//                   as if the current final result is the start of a new route."]
//         Immediate(SvRouteAddImmediate: { () } -> { () }),

//         #[template = "-i|interactive"]
//         #[help = "Interactively provide targets to generate a route with by clicking on vertices in the visualizer with your mouse. \
//                   Run the command `sv.route` without arguments when you are finished selecting targets."]
//         Interactive(SvRouteAddInteractive: { () } -> { () }),
//     },

//     #[input = "sv.route.list"]
//     #[help = "List the results of the current route."]
//     SvRouteList {
//         #[template = ""]
//         #[help = "Print the results of the current route in order or pass them as arguments to the next command in the pipeline."]
//         Basic(SvRouteListBasic: { () } -> { () }),
//     },

//     #[input = "sv.route.clear"]
//     #[help = "Clear the current route."]
//     SvRouteClear {
//         #[template = ""]
//         #[help = "Remove all targets, results, and visuals from the current route."]
//         Basic(SvRouteClearBasic: { () } -> { () }),
//     },

//     #[input = "sv.new"]
//     #[help = "Create a new vertex."]
//     SvNew {
//         #[template = "<ID> [ALIAS] x:<???>/y:<???>[/z:<???>]"]
//         #[help = "Create a new vertex with the specified name/alias at the specified position. It will start with no edges. The alias can be the same as the ID, \
//                   but neither the ID nor alias may match the ID of any existing vertex, nor may either match the alias of any existing vertex."]
//         Basic(SvNewBasic: { () } -> { () }),
//     },

//     #[input = "sv.edge"]
//     #[help = "Create a new edge."]
//     SvEdge {
//         #[template = "<ID|ALIAS> [--] <ID|ALIAS>..."]
//         #[help = "Create two-way edges connecting the prior vertex to each of the following vertices not separated by `--`s. A vertex cannot have an edge to itself. \
//                   When one or more vertices not separated with `--`s are followed by a `--`, each vertex in that subset will be connected to each vertex in the following subset."]
//         Basic(SvEdgeBasic: { () } -> { () }),
//     },

//     #[input = "sv.load"]
//     #[help = "Load a graph from a file."]
//     SvLoad {
//         #[template = "<PATH>"]
//         #[help = "Load a weight graph map from the specified file path."]
//         Basic(SvLoadBasic: { () } -> { () }),
//     },

//     #[input = "sv.save"]
//     #[help = "Save the graph to a file."]
//     SvSave {
//         #[template = "<PATH>"]
//         #[help = "Save the current weighted graph map to the specified file path. If the file does not exist, it will be created."]
//         Basic(SvSaveBasic: { () } -> { () }),
//     },

//     #[input = "tempo"]
//     #[help = "Set the tempo of the route generator."]
//     Tempo {
//         #[template = "reset|sync|sprint|instant|pause|ticks:<???>/ms:<???>"]
//         #[help = "Set the rate at which the route generator performs update steps in ticks per millisecond. \
//                   Ticks can only occur during frames, so if the 'ms' are lower the duration of a single frame, \
//                   multiple ticks will occur during a frame and only the latest one will be visualized in that frame.\n\
//                   - `reset`: set the tempo to 1 tick per millisecond.\n\
//                   - `sync`: set the tempo to 1 tick per frame.\n\
//                   - `sprint`: set the tempo to the fastest possible while keeping the current framerate.\n\
//                   - `instant`: set the tempo to 1 tick per 0 milliseconds, blocking until the route is completed.\n\
//                   - `pause`: set the tempo to 0 ticks per millisecond."]
//         Basic(TempoBasic: { () } -> { () }),
//     },

//     #[input = "skip"]
//     #[help = "Repeat all but the first n arguments."]
//     Skip {
//         #[template = "<N> [ANY]..."]
//         #[help = "Pass all arguments after the first `n` arguments (after the argument for `n`) as arguments to the next command in the pipeline."]
//         Basic(SkipBasic: { () } -> { () }),
//     },

//     #[input = "take"]
//     #[help = "Only repeat the first n arguments."]
//     Take {
//         #[template = "<N> [ANY]..."]
//         #[help = "Pass the first `n` arguments (after the argument for `n`) as arguments to the next command in the pipeline."]
//         Basic(TakeBasic: { () } -> { () }),
//     },
// }

// struct HelpAll;

// impl Command for HelpAll {
//     type Args;
//     type Rets;
//     type Err = !;

//     fn run(cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData, args: Self::Args) -> Self::Rets {
//         todo!()
//     }
// }

// struct HelpOne;

// impl Command for HelpOne {

// }

// struct CloseBasic;

// impl Command for CloseBasic {

// }

// struct ClsBasic;

// impl Command for ClsBasic {

// }

// struct EchoBasic;

// impl Command for EchoBasic {

// }

// struct DbgToggle;

// impl Command for DbgToggle {

// }

// struct DbgSet;

// impl Command for DbgSet {

// }

// struct FocusVertex;

// impl Command for FocusVertex {

// }

// struct FocusReset;

// impl Command for FocusReset {

// }

// struct FocusPrint;

// impl Command for FocusPrint {

// }

// struct ColorVertsGeneral;

// impl Command for ColorVertsGeneral {

// }

// struct ColorVertsSpecific;

// impl Command for ColorVertsSpecific {

// }

// struct ColorEdgesBasic;

// impl Command for ColorEdgesBasic {

// }

// struct ColorBackgroundBasic;

// impl Command for ColorBackgroundBasic {

// }

// struct SvRouteImmediate;

// impl Command for SvRouteImmediate {

// }

// struct SvRouteInteractive;

// impl Command for SvRouteInteractive {

// }

// struct SvRouteAddImmediate;

// impl Command for SvRouteAddImmediate {

// }

// struct SvRouteAddInteractive;

// impl Command for SvRouteAddInteractive {

// }

// struct SvRouteListBasic;

// impl Command for SvRouteListBasic {

// }

// struct SvRouteClearBasic;

// impl Command for SvRouteClearBasic {

// }

// struct SvNewBasic;

// impl Command for SvNewBasic {

// }

// struct SvEdgeBasic;

// impl Command for SvEdgeBasic {

// }

// struct SvLoadBasic;

// impl Command for SvLoadBasic {

// }

// struct SvSaveBasic;

// impl Command for SvSaveBasic {

// }

// struct TempoBasic;

// impl Command for TempoBasic {

// }

// struct SkipBasic;

// impl Command for SkipBasic {

// }

// struct TakeBasic;

// impl Command for TakeBasic {

// }





// pub struct CmdPromise {
//     poll: Box<dyn FnMut(&mut ConsoleOut, &mut ConsoleIn, &mut ProgramData) -> Poll<Result<Return, CmdError>>>,
// }

// impl From<Result<Return, CmdError>> for Promise {
//     fn from(value: Result<Return, CmdError>) -> Self {
//         Self::ready(value)
//     }
// }

// impl Promise {
//     pub fn ready(value: Result<Return, CmdError>) -> Self {
//         Self::pending(move |_, _, _| Poll::Ready(value))
//     }

//     pub fn pending(poll_fn: impl FnMut(&mut ConsoleOut, &mut ConsoleIn, &mut ProgramData) -> Poll<Result<Return, CmdError>> + 'static) -> Self {
//         Self { poll: Box::new(poll_fn) }
//     }

//     pub fn poll(self, cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData) -> Poll<Result<Return, CmdError>> {
//         match self {
//             Promise::Ready(value) => Poll::Ready(std::mem::take(value)),
//             Promise::Pending(poll) => poll(cout, cin, data),
//         }

//         // match self {
//         //     Self::Ready(x) => Poll::Ready(Ok(std::mem::take(x))),
//         //     Self::InteractiveTargets => {
//         //         if data.is_giving_interactive_targets {
//         //             Poll::Pending
//         //         } else {
//         //             let mut targets = std::mem::take(&mut data.interactive_targets).into_iter();
//         //             if let Some(start) = targets.next() {
//         //                 data.route = Some(RouteGenerator::new(data.graph.verts().len(), start, targets));
//         //                 console_log!(cout, Info, "generating route...");
//         //                 CmdPromise::Route.poll(cout, cin, data)
//         //             } else {
//         //                 // todo: obviously this isn't a parse error, this deserves its own error
//         //                 Poll::Ready(Err(CmdError::VertexDNE(String::new())))
//         //             }
//         //         }
//         //     }
//         //     Self::Route => {
//         //         if let Some(route) = data.route.as_ref() {
//         //             if route.is_finished() {
//         //                 let results = route.result().iter()
//         //                     .map(|&v| data.graph.vert(v).id.clone())
//         //                     .collect::<Vec<String>>();

//         //                 Poll::Ready(Ok(CmdReturn::new(results, |cout, _, results| console_log!(cout, Route, "{}", results.join(" - ")))))
//         //                     // let mut distance = 0.0;
//         //                     // route.visited.fill(const { None });
//         //                     // route.visited[results[0] as usize] = Some(Visit { distance, parent: None });
//         //                     // if *i + 1 < results.len() {
//         //                     //     let a = &results[*i];
//         //                     //     let b = &results[*i + 1];
//         //                     //     distance += graph.adjacent(*a).iter()
//         //                     //         .find_map(|e| (&e.vertex == b).then_some(e.weight))
//         //                     //         .expect("results should be adjacent");
//         //                     //     route.visited[*b as usize] = Some(Visit { distance, parent: Some(*a) });
//         //                     //     *i += 1;
//         //                     // }
//         //                     // console_log!(cout, Route, "total distance: {distance}");
//         //             } else {
//         //                 Poll::Pending
//         //             }
//         //         } else {
//         //             Poll::Ready(Err(CmdError::NoExistingRoute))
//         //         }
//         //     }
//         // }
//     }
// }

// fn help_all_msg() -> String {
//     let cmd_column_width = Cmd::LIST.into_iter()
//         .map(|cmd| cmd.input().len())
//         .max()
//         .unwrap() + 2;

//     let mut msg = String::with_capacity(2048);
//     msg.push_str("<color = #48f19d>Commands:</color>");
//     for cmd in &Cmd::LIST {
//         msg.push_str("\n    <color = #0096e6>");
//         let input = cmd.input();
//         msg.push_str(cmd.input());
//         msg.push_str("</color>");
//         for _ in input.len()..cmd_column_width {
//             msg.push(' ');
//         }
//         msg.push_str(cmd.help());
//     }
//     msg
// }

// fn run_help_all(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if args.is_empty() {
//         Ok(Promise::Ready(Return::disp(|cout, _| console_log!(cout, Info, "{}", help_all_msg()))))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Help))
//     }
// }

// impl Cmd {
//     fn help_msg(self) -> String {
//         let template_column_width = self.usage_list().into_iter()
//             .map(|usage| self.input().len() + if !usage.template().is_empty() { usage.template().len() + 1 } else { 0 })
//             .max()
//             .unwrap() + 2;

//         let mut msg = String::with_capacity(2048);
//         msg.push_str("<color = #48f19d>Usage:</color>");
//         for usage in self.usage_list() {
//             msg.push_str("\n    <color = #0096e6>");
//             let input = self.input();
//             msg.push_str(input);
//             msg.push_str("</color>");
//             let template = usage.template();
//             let column_used = if !template.is_empty() {
//                 msg.push_str(" <color = #0096e6aa>");
//                 msg.push_str(template);
//                 msg.push_str("</color>");
//                 input.len() + template.len() + 1
//             } else {
//                 input.len()
//             };
//             let mut lines_it = usage.help().lines();
//             if let Some(line) = lines_it.next() {
//                 for _ in column_used..template_column_width {
//                     msg.push(' ');
//                 }
//                 msg.push_str(line);

//                 for line in lines_it {
//                     msg.push_str("\n    ");
//                     for _ in 0..template_column_width {
//                         msg.push(' ');
//                     }
//                     msg.push_str(line);
//                 }
//             }
//         }
//         msg
//     }
// }

// fn run_help_one(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [cmd] = args {
//         let cmd = cmd.parse::<Cmd>()
//             .map_err(|_| CmdError::CheckUsage(Cmd::Help))?;

//         Ok(Promise::Ready(Return::disp(move |cout, _| console_log!(cout, Info, "{}", cmd.help_msg()))))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Help))
//     }
// }

// fn run_close(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if args.is_empty() {
//         data.should_close = true;
//         Ok(Promise::Ready(Return::void()))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Cls))
//     }
// }

// fn run_cls(cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if args.is_empty() {
//         cout.clear_log();
//         Ok(Promise::Ready(Return::void()))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Cls))
//     }
// }

// fn run_echo(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Ok(Promise::Ready(Return::new(args.into_iter().map(<&str>::to_string).collect(), |cout, _, args| console_log!(cout, Info, "{}", args.join(" ")))))
// }

// fn run_dbg_toggle(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if args.is_empty() {
//         data.is_debugging = !data.is_debugging;
//         Ok(Promise::Ready(Return::new(
//             vec![if data.is_debugging { "true" } else { "false" }.to_string()],
//             |cout, data, _| console_log!(cout, Info, "debug messages are now {}", if data.is_debugging { "enabled" } else { "disabled" }),
//         )))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Dbg))
//     }
// }

// fn run_dbg_set(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [set_arg] = args {
//         data.is_debugging = set_arg.parse::<bool>()
//             .map_err(|e| CmdError::ParseBool(e))?;

//         Ok(Promise::Ready(Return::new(
//             vec![if data.is_debugging { "true" } else { "false" }.to_string()],
//             |cout, data, _| console_log!(cout, Info, "debug messages are now {}", if data.is_debugging { "enabled" } else { "disabled" }),
//         )))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Dbg))
//     }
// }

// fn run_focus_vertex(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [target_arg] = args && &**target_arg != "reset" {
//         let vert = data.graph.verts().iter()
//             .find(|vert| (vert.id.eq_ignore_ascii_case(target_arg) || vert.alias.eq_ignore_ascii_case(target_arg)))
//             .ok_or_else(|| CmdError::VertexDNE(target_arg.to_string()))?;

//         data.orbit.target = vert.pos;
//         data.orbit.length = 400.0;
//         Ok(Promise::Ready(Return::new(vec![vert.alias.clone()], |cout, _, vertex| console_log!(cout, Info, "focused vertex {}", vertex[0]))))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Focus))
//     }
// }

// fn run_focus_reset(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if matches!(args, ["reset"]) {
//         data.orbit.target = Vector3::zero();
//         data.orbit.length = CAMERA_LENGTH_DEFAULT;
//         Ok(Promise::Ready(Return::void()))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Focus))
//     }
// }

// fn run_focus_print(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if args.is_empty() {
//         let vert = data.graph.verts().iter()
//             .find(|vert| check_collision_spheres(vert.pos, VERTEX_RADIUS, data.orbit.target, 1.0));

//         Ok(Promise::Ready(Return::new(
//             vert.into_iter().map(|target| target.id.clone()).collect::<Vec<_>>(),
//             |cout, _, focused| if focused.is_empty() {
//                 console_log!(cout, Info, "no vertex is currently focused");
//             } else {
//                 console_log!(cout, Info, "currently focusing vertex {}", focused[0]);
//             }
//         )))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Focus))
//     }
// }

// fn run_color_verts_general(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [color_arg] = args {
//         data.verts_color = color_arg.parse::<RichColor>()
//             .map(|RichColor(c)| c)
//             .map_err(|e| CmdError::ParseColor(e))?;
//         Ok(Promise::Ready(Return::disp(|cout, _| console_log!(cout, Info, "updated vertex color"))))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::ColorVerts))
//     }
// }

// fn run_color_verts_specific(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, _args: &[&str]) -> Result<Promise, CmdError> {
//     todo!("run_color_verts_specific requires per-vertex colors")
// }

// fn run_color_edges(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [color_arg] = args {
//         data.edges_color = color_arg.parse::<RichColor>()
//             .map(|RichColor(c)| c)
//             .map_err(|e| CmdError::ParseColor(e))?;
//         Ok(Promise::Ready(Return::disp(|cout, _| console_log!(cout, Info, "updated edge color"))))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::ColorVerts))
//     }
// }

// fn run_color_background(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [color_arg] = args {
//         data.background_color = color_arg.parse::<RichColor>()
//             .map(|RichColor(c)| c)
//             .map_err(|e| CmdError::ParseColor(e))?;
//         Ok(Promise::Ready(Return::disp(|cout, _| console_log!(cout, Info, "updated background color"))))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::ColorVerts))
//     }
// }

// fn run_sv_route_immediate(cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [start, targets @ ..] = args && !targets.is_empty() && !matches!(&**start, "-i"|"interactive") {
//         let start = start.parse_vert(&data.graph)?;
//         let targets = args.iter()
//             .map(|s| s.parse_vert(&data.graph))
//             .collect::<Result<Vec<VertexID>, CmdError>>()?;

//         data.route = Some(RouteGenerator::new(data.graph.verts().len(), start, targets));
//         console_log!(cout, Info, "generating route...");
//         Ok(Promise::Route)
//     } else {
//         Err(CmdError::CheckUsage(Cmd::SvRoute))
//     }
// }

// fn run_sv_route_interactive(cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if matches!(args, ["-i"|"interactive"]) {
//         console_log!(cout, Info,
//             "click each target with the mouse; order doesn't matter except that the first will be the start \n\
//              click a target again to un-target it\n\
//              run the command `<color = #0096e6>sv.route</color>` (without arguments) when finished");
//         cin.insert_over_selection("sv.route");
//         data.is_giving_interactive_targets = true;
//         Ok(Promise::InteractiveTargets)
//     } else if args.is_empty() && data.is_giving_interactive_targets {
//         data.is_giving_interactive_targets = false;
//         Ok(Promise::Ready(Return::void()))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::SvRoute))
//     }
// }

// fn run_sv_route_add_immediate(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Err(CmdError::Todo)
// }

// fn run_sv_route_add_interactive(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Err(CmdError::Todo)
// }

// fn run_sv_route_list(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if args.is_empty() {
//         if let Some(route) = &data.route {
//             let list = route.result().iter()
//                 .copied()
//                 .map(|v| data.graph.vert(v).id.clone())
//                 .collect();
//             Ok(Promise::Ready(Return::new(list, |cout, _, list| console_log!(cout, Route, "{}", list.join(" - ")))))
//                 // let mut distance = 0.0;
//                 // route.visited.fill(const { None });
//                 // route.visited[results[0] as usize] = Some(Visit { distance, parent: None });
//                 // if *i + 1 < results.len() {
//                 //     let a = &results[*i];
//                 //     let b = &results[*i + 1];
//                 //     distance += graph.adjacent(*a).iter()
//                 //         .find_map(|e| (&e.vertex == b).then_some(e.weight))
//                 //         .expect("results should be adjacent");
//                 //     route.visited[*b as usize] = Some(Visit { distance, parent: Some(*a) });
//                 //     *i += 1;
//                 // }
//                 // console_log!(cout, Route, "total distance: {distance}");
//         } else {
//             Err(CmdError::NoExistingRoute)
//         }
//     } else {
//         Err(CmdError::CheckUsage(Cmd::SvRouteList))
//     }
// }

// fn run_sv_route_clear(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Err(CmdError::Todo)
// }

// fn run_sv_new(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Err(CmdError::Todo)
// }

// fn run_sv_edge(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Err(CmdError::Todo)
// }

// fn run_sv_load(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Err(CmdError::Todo)
// }

// fn run_sv_save(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Err(CmdError::Todo)
// }

// fn run_tempo(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     Err(CmdError::Todo)
// }

// fn run_skip(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [n_arg, rest @ ..] = args && n_arg.chars().all(char::is_numeric) {
//         let n = n_arg.parse::<usize>().map_err(|e| CmdError::ParseInt(e))?;
//         Ok(Promise::Ready(Return::pure(rest.into_iter().skip(n).map(<&str>::to_string).collect())))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Skip))
//     }
// }

// fn run_take(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: &[&str]) -> Result<Promise, CmdError> {
//     if let [n_arg, rest @ ..] = args && n_arg.chars().all(char::is_numeric) {
//         let n = n_arg.parse::<usize>().map_err(|e| CmdError::ParseInt(e))?;
//         Ok(Promise::Ready(Return::pure(rest.into_iter().take(n).map(<&str>::to_string).collect())))
//     } else {
//         Err(CmdError::CheckUsage(Cmd::Take))
//     }
// }


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
