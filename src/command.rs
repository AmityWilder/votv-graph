use std::{num::NonZeroU32, path::Path};
use raylib::prelude::*;
use crate::{console::{input::ConsoleIn, output::ConsoleOut}, console_log, graph::{Adjacent, VertexID, WeightedGraph}, route::RouteGenerator, serialization::LoadGraphError, CAMERA_POSITION_DEFAULT, VERTEX_RADIUS};

pub enum Tempo {
    Sync,
    Sprint,
    Instant,
    Paused,
    Exact {
        ticks: NonZeroU32,
        ms: NonZeroU32,
    },
}
impl Default for Tempo {
    fn default() -> Self {
        Self::new()
    }
}
impl Tempo {
    pub const fn new() -> Self {
        Self::Exact {
            ticks: unsafe { NonZeroU32::new_unchecked(1) },
            ms: unsafe { NonZeroU32::new_unchecked(1) },
        }
    }
}
impl std::fmt::Display for Tempo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tempo::Sync => f.write_str("FPS sync"),
            Tempo::Sprint => f.write_str("max responsive"),
            Tempo::Instant => f.write_str("run in one frame"),
            Tempo::Paused => f.write_str("paused"),
            Tempo::Exact { ticks, ms } => write!(f, "{ticks} steps every {ms}ms"),
        }
    }
}

#[derive(Debug)]
pub enum ParseCoordsError {
    Syntax,
    ParseFloat(std::num::ParseFloatError),
}
impl std::fmt::Display for ParseCoordsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseCoordsError::Syntax => f.write_str("expected x:<???>/y:<???>"),
            ParseCoordsError::ParseFloat(_) => f.write_str("failed to read number"),
        }
    }
}
impl std::error::Error for ParseCoordsError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseCoordsError::Syntax => None,
            ParseCoordsError::ParseFloat(e) => Some(e),
        }
    }
}

pub fn parse_coords(coords: &str) -> Result<Vector3, ParseCoordsError> {
    let (x, y) = coords.split_once('/').ok_or(ParseCoordsError::Syntax)?;
    let x = if x.starts_with("x:") { x[2..].parse().map_err(|e| ParseCoordsError::ParseFloat(e))? } else { return Err(ParseCoordsError::Syntax) };
    let y = if y.starts_with("y:") { y[2..].parse().map_err(|e| ParseCoordsError::ParseFloat(e))? } else { return Err(ParseCoordsError::Syntax) };
    Ok(Vector3::new(x, 0.0, y))
}

#[derive(Debug)]
pub enum FromCmdError {
    Unknown(String),
}
impl std::fmt::Display for FromCmdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown(cmd) => write!(f, "The term '{cmd}' is not recognized as the name of a cmdlet, function, script file, or operable program. Check the spelling of the name, or if a path was included, verify that the path is correct and try again."),
        }
    }
}
impl std::error::Error for FromCmdError {}

macro_rules! define_commands {
    (
        $(#[$enum_meta:meta])*
        $vis:vis enum $Enum:ident {$(
            #[input($input:literal $(,)?)]
            #[usage($(case(args($($args:literal),* $(,)?), desc = $description:literal)),+ $(,)?)]
            $(#[$variant_meta:meta])*
            $Variant:ident
        ),* $(,)?}
    ) => {
        $(#[$enum_meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $vis enum $Enum {$(
            $(#[$variant_meta])*
            #[doc = concat!(" # Command `", $input, "`\n ## Usage", $("\n - `", $input, $(" ", $args,)* "`: ", $description),+)]
            $Variant,
        )+}
        impl $Enum {
            pub const LIST: &[Self] = &[
                $(Self::$Variant,)+
            ];
            pub fn try_from_str(s: &str) -> Result<Self, FromCmdError> {
                match s {
                    $($input => Ok(Self::$Variant),)+
                    _ => Err(FromCmdError::Unknown(s.to_string())),
                }
            }
            pub const fn input(&self) -> &'static str {
                match self {
                    $(Self::$Variant => $input,)+
                }
            }
            pub const fn args(&self) -> &'static [&'static str] {
                match self {
                    $(Self::$Variant => &[$(concat!($(" ", $args),*),)*],)+
                }
            }
            pub const fn usage(&self) -> &'static [&'static str] {
                match self {
                    $(Self::$Variant => &[$(concat!($input, $(" ", $args),*),)*],)+
                }
            }
            pub const fn description(&self) -> &'static [&'static str] {
                match self {
                    $(Self::$Variant => &[$($description,)+],)+
                }
            }
        }
    };
}

define_commands!{
    pub enum Cmd {
        #[input("help")]
        #[usage(case(args(), desc = "Display this information"))]
        Help,

        #[input("close")]
        #[usage(case(args(), desc = "Close the application"))]
        Close,

        #[input("cls")]
        #[usage(case(args(), desc = "Clear the console"))]
        Cls,

        #[input("echo")]
        #[usage(case(args(), desc = "Print the input to the console"))]
        Echo,

        #[input("await")]
        #[usage(case(args(), desc = "Execute the arguments as an asynchronous command, blocking until complete"))]
        Await,

        #[input("dbg")]
        #[usage(case(args(), desc = "Toggle debug messages"))]
        Dbg,

        #[input("focus")]
        #[usage(
            case(args("<ID|ALIAS>"), desc = "Zoom in on a particular target"),
            case(args("reset"),      desc = "Reset camera orientation"),
            case(args(),             desc = "Print the name of the focused vertex"),
        )]
        Focus,

        #[input("sv.route")]
        #[usage(
            case(args("<START>", "<ID|ALIAS>..."), desc = "Generate the shortest route visiting each target (separated by spaces)"),
            case(args("-i|interactive"),           desc = "Provide targets to the route generator through the graphic interface"),
        )]
        SvRoute,

        #[input("sv.route.add")]
        #[usage(
            case(args("<ID|ALIAS>..."),  desc = "Add more targets (separated by spaces) to the current route"),
            case(args("-i|interactive"), desc = "Add more targets to the current route through the graphic interface"),
        )]
        SvRouteAdd,

        #[input("sv.route.list")]
        #[usage(case(args(), desc = "List the order of the targets in the current route"))]
        SvRouteList,

        #[input("sv.route.clear")]
        #[usage(case(args(), desc = "Clear the ongoing route"))]
        SvRouteClear,

        #[input("sv.new")]
        #[usage(
            case(args("<ID>", "[ALIAS]", "x:<???>/y:<???>"), desc = "Create a new vertex that can be targeted at x,y"),
            case(args("<ID>", "[ALIAS]", "focus"),           desc = "Create a new vertex that can be targeted at the focused position"),
        )]
        SvNew,

        #[input("sv.edge")]
        #[usage(
            case(args("<ID|ALIAS>", "<ID|ALIAS>"), desc = "Create an edge connecting two existing vertices"),
            case(args("<ID|ALIAS>"),               desc = "Print a list of all vertices adjacent to the target"),
        )]
        SvEdge,

        #[input("sv.load")]
        #[usage(case(args("<PATH>"), desc = "Load another map from a graph file"))]
        SvLoad,

        #[input("sv.save")]
        #[usage(case(args("<PATH>"), desc = "Save the current map to a graph file"))]
        SvSave,

        #[input("tempo")]
        #[usage(
            case(args("<TICKS>/<MILLISECONDS>"), desc = "Set the route tick speed in ticks per milliseconds"),
            case(args("reset"),                  desc = "Set the route tick speed to the default (1 step per millisecond)"),
            case(args("sync"),                   desc = "Sync the route tick speed with the framerate"),
            case(args("sprint"),                 desc = "Set the route tick speed to the maximum (1 step every 0 milliseconds)"),
            case(args(),                         desc = "Print the current tempo"),
        )]
        Tempo,

        #[input("skip")]
        #[usage(case(args("<COUNT> [ANY]..."), desc = "Skip the specified number of arguments and pass the rest as output"))]
        Skip,

        #[input("take")]
        #[usage(case(args("<COUNT> [ANY]..."), desc = "Return the specified number of arguments"))]
        Take,
    }
}

#[derive(Debug)]
pub enum CmdError {
    CheckUsage(Cmd),
    VertexDNE(String),
    MissingArgs(&'static str),
    NoExistingRoute,
    ParseCoordsFailed(ParseCoordsError),
    ParseTempoTicksFailed(std::num::ParseIntError),
    ParseTempoMillisecondsFailed(std::num::ParseIntError),
    LoadGraphFailed(LoadGraphError),
    IOError(std::io::Error),
}
impl std::fmt::Display for CmdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::CheckUsage(cmd) => {
                let usage = cmd.usage();
                let has_multiple = usage.len() > 1;
                write!(f, "usage:{}{}", if has_multiple { "\n    " } else { " " }, usage.join("\n    "))
            }
            Self::VertexDNE(id) => write!(f, "vertex \"{id}\" does not exist"),
            Self::MissingArgs(args) => write!(f, "missing input for {args}"),
            Self::NoExistingRoute => f.write_str("no ongoing route to extend"),
            Self::ParseCoordsFailed(_) => f.write_str("could not parse coordinates"),
            Self::ParseTempoTicksFailed(_) => f.write_str("could not parse ticks"),
            Self::ParseTempoMillisecondsFailed(_) => f.write_str("could not parse milliseconds"),
            Self::LoadGraphFailed(_) => f.write_str("could not load graph"),
            Self::IOError(_) => f.write_str("filesystem IO error"),
        }
    }
}
impl std::error::Error for CmdError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            | Self::CheckUsage(_)
            | Self::VertexDNE(_)
            | Self::MissingArgs(_)
            | Self::NoExistingRoute
                => None,

            Self::ParseCoordsFailed(e) => Some(e),
            Self::LoadGraphFailed(e) => Some(e),

            | Self::ParseTempoTicksFailed(e)
            | Self::ParseTempoMillisecondsFailed(e)
                => Some(e),

            Self::IOError(e) => Some(e),
        }
    }
}
impl From<std::io::Error> for CmdError {
    fn from(value: std::io::Error) -> Self {
        CmdError::IOError(value)
    }
}

#[derive(Debug)]
pub enum CmdMessage {
    None,
    Return(String),
    RequestInteractiveTargets,
    Exit,
}

pub type CmdResult = Result<CmdMessage, CmdError>;

impl Cmd {
    fn parse(s: &str) -> Option<Result<(Cmd, std::str::SplitWhitespace<'_>), FromCmdError>> {
        let mut args = s.split_whitespace();
        args.next()
            .map(|first|
                Cmd::try_from_str(first)
                    .map(|cmd| (cmd, args))
            )
    }

    pub fn run_focus<'g, 'args>(
        cout: &mut ConsoleOut,
        graph: &'g WeightedGraph,
        camera: &mut Camera3D,
        mut args: impl Iterator<Item = &'args str>,
    ) -> CmdResult {
        if let Some(target) = args.next() {
            if target == "reset" {
                camera.position = CAMERA_POSITION_DEFAULT;
                camera.target = Vector3::zero();
                Ok(CmdMessage::None)
            } else {
                let vert = graph.verts().iter()
                    .find(|vert| (vert.id.eq_ignore_ascii_case(target) || vert.alias.eq_ignore_ascii_case(target)));

                if let Some(vert) = vert {
                    camera.target = vert.pos;
                    camera.position = camera.target + Vector3::new(0.0, 400.0, 0.0);
                    Ok(CmdMessage::None)
                } else {
                    Err(CmdError::VertexDNE(target.to_string()))
                }
            }
        } else {
            let vert = graph.verts().iter()
                .find(|vert| check_collision_spheres(vert.pos, VERTEX_RADIUS, camera.target, 1.0));

            let id = if let Some(target) = vert {
                console_log!(cout, Info, "currently focusing vertex {}", target.id);
                target.id.as_str()
            } else {
                console_log!(cout, Info, "no vertex is currently focused");
                ""
            };
            Ok(CmdMessage::Return(id.to_string()))
        }
    }

    pub fn run_help(cout: &mut ConsoleOut) {
        let usages = Cmd::LIST.into_iter().copied()
            .flat_map(|item|
                item.args().into_iter().copied()
                    .map(move |args| (item.input(), args))
            )
            .map(|(input, args)| format!("<color = #0096e6>{input}</color><color = #0096e6aa>{args}</color>"))
            .collect::<Vec<_>>();

        let width = usages.iter()
            .map(|item| item.len())
            .max().expect("should have at least one item");

        let lines = usages.iter().map(String::as_str)
            .zip(Cmd::LIST.into_iter().copied()
                .flat_map(|item| item.description().into_iter()).copied()
            )
            .map(|(usage, desc)| format!("{usage:<0$}  {desc}", width))
            .collect::<Vec<_>>();

        let msg =
            std::iter::once("<color = #48f19d>Commands:</color>")
                .chain(lines.iter().map(String::as_str))
                .chain([
                    "\n<color = #48f19d>Operators:</color>",
                    "<color = #0096e6>|</color>                                    Perform two commands sequentially, sending the output of the first as arguments to the second",
                ].into_iter())
                .collect::<Vec<_>>()
                .join("\n    ");

        console_log!(cout, Info, "{msg}");
    }

    pub fn run_sv_route<'args>(
        cout: &mut ConsoleOut,
        cin: &mut ConsoleIn,
        graph: &WeightedGraph,
        route: &mut Option<RouteGenerator>,
        interactive_targets: Vec<VertexID>,
        args: impl Iterator<Item = &'args str>,
    ) -> CmdResult {
        let mut args = args.peekable();
        let targets = match args.peek() {
            Some(&("-i" | "interactive")) => {
                console_log!(cout, Info, "click each target with the mouse; order doesn't matter except that the first will be the start");
                console_log!(cout, Info, "click a target again to un-target it");
                console_log!(cout, Info, "run the command `<color = #0096e6>sv.route</color>` (without arguments) when finished");
                cin.insert_over_selection("sv.route");
                return Ok(CmdMessage::RequestInteractiveTargets);
            }
            Some(_) => {
                args
                    .map(|id| graph.find_vert(id).map_err(|start| CmdError::VertexDNE(start.to_string())))
                    .collect::<Result<Vec<VertexID>, CmdError>>()?
            },
            None => interactive_targets,
        };

        let mut target_iter = targets.into_iter().peekable();
        let start = target_iter.next().ok_or(CmdError::CheckUsage(Cmd::SvRoute))?;
        if target_iter.peek().is_none() {
            return Err(CmdError::MissingArgs("targets"));
        }
        *route = Some(RouteGenerator::new(graph.verts().len(), start, target_iter));
        console_log!(cout, Info, "generating route");
        Ok(CmdMessage::None)
    }

    pub fn run_sv_route_add<'args>(
        cout: &mut ConsoleOut,
        cin: &mut ConsoleIn,
        graph: &WeightedGraph,
        route: &mut Option<RouteGenerator>,
        interactive_targets: Vec<VertexID>,
        args: impl Iterator<Item = &'args str>,
    ) -> CmdResult {
        let route = route.as_mut().ok_or(CmdError::NoExistingRoute)?;
        let mut args = args.peekable();
        let targets = match args.peek() {
            Some(&("-i" | "interactive")) => {
                console_log!(cout, Info, "click each target with the mouse; order doesn't matter");
                console_log!(cout, Info, "click a target again to un-target it");
                console_log!(cout, Info, "run the command `<color = #0096e6>sv.route.add</color>` (without arguments) when finished");
                cin.insert_over_selection("sv.route.add");
                return Ok(CmdMessage::RequestInteractiveTargets);
            }
            Some(_) => {
                args
                    .map(|id| graph.find_vert(id).map_err(|start| CmdError::VertexDNE(start.to_string())))
                    .collect::<Result<Vec<VertexID>, CmdError>>()?
            },
            None => interactive_targets,
        };
        if targets.is_empty() {
            return Err(CmdError::MissingArgs("targets"));
        }
        route.add_targets(cout, targets);
        console_log!(cout, Info, "extending route");
        Ok(CmdMessage::None)
    }

    pub fn run_sv_new<'args>(
        cout: &mut ConsoleOut,
        graph: &mut WeightedGraph,
        route: &mut Option<RouteGenerator>,
        camera: Camera3D,
        mut args: impl Iterator<Item = &'args str>,
    ) -> CmdResult {
        let id = args.next().ok_or(CmdError::CheckUsage(Cmd::SvNew))?;
        let mut alias = args.next();
        let coords = args.next().or_else(|| alias.take()).ok_or(CmdError::CheckUsage(Cmd::SvNew))?;
        let pos = if coords == "focus" {
            camera.target
        } else {
            parse_coords(coords).map_err(|e| CmdError::ParseCoordsFailed(e))?
        };
        graph.add_vertex(id, alias.unwrap_or(id), pos);
        *route = None;
        console_log!(cout, Info, "created new vertex");
        Ok(CmdMessage::None)
    }

    pub fn run_sv_edge<'args>(
        cout: &mut ConsoleOut,
        graph: &mut WeightedGraph,
        route: &mut Option<RouteGenerator>,
        mut args: impl Iterator<Item = &'args str>,
    ) -> CmdResult {
        let a = args.next().ok_or(CmdError::CheckUsage(Cmd::SvEdge))?;
        let b = args.next().ok_or(CmdError::CheckUsage(Cmd::SvEdge))?;
        let a = graph.find_vert(a).map_err(|id| CmdError::VertexDNE(id.to_string()))?;
        let b = graph.find_vert(b).map_err(|id| CmdError::VertexDNE(id.to_string()))?;
        if graph.adjacent(a).iter().any(|Adjacent { vertex, .. }| vertex == &b) {
            console_log!(cout, Warning, "vertices {a} and {b} are already connected");
        } else {
            graph.add_edge(a, b);
            *route = None;
            console_log!(cout, Info, "created an edge connecting vertices {a} and {b}");
        }
        Ok(CmdMessage::None)
    }

    pub fn run_tempo<'args>(
        cout: &mut ConsoleOut,
        tempo: &mut Tempo,
        mut args: impl Iterator<Item = &'args str>,
    ) -> CmdResult {
        if let Some(arg) = args.next() {
            if arg == "reset" {
                *tempo = Tempo::new();
            } else if arg == "sync" {
                *tempo = Tempo::Sync;
            } else if arg == "sprint" {
                *tempo = Tempo::Sprint;
            } else if let Some((ticks, ms)) = arg.split_once('/') {
                let ticks = NonZeroU32::new(ticks.parse().map_err(|e| CmdError::ParseTempoTicksFailed(e))?);
                let ms = NonZeroU32::new(ms.parse().map_err(|e| CmdError::ParseTempoMillisecondsFailed(e))?);
                *tempo = match (ticks, ms) {
                    (None, _) => Tempo::Paused,
                    (Some(_), None) => Tempo::Instant,
                    (Some(ticks), Some(ms)) => Tempo::Exact { ticks, ms },
                };
            } else {
                return Err(CmdError::CheckUsage(Cmd::Tempo));
            }
            console_log!(cout, Info, "set tempo to {tempo}");
        } else {
            console_log!(cout, Info, "current tempo is {tempo}");
        }
        Ok(CmdMessage::None)
    }

    pub fn run_sv_load<'args>(
        cout: &mut ConsoleOut,
        graph: &mut WeightedGraph,
        route: &mut Option<RouteGenerator>,
        mut args: impl Iterator<Item = &'args str>,
    ) -> CmdResult {
        if let Some(path_str) = args.next() {
            let path = Path::new(path_str);
            let mem = std::fs::read_to_string(path)?;
            *graph = WeightedGraph::load_from_memory(mem).map_err(|e| CmdError::LoadGraphFailed(e))?;
            *route = None;
            console_log!(cout, Info, "graph loaded from \"{path_str}\"");
            Ok(CmdMessage::None)
        } else {
            Err(CmdError::CheckUsage(Cmd::SvLoad))
        }
    }

    pub fn run_sv_save<'args>(
        cout: &mut ConsoleOut,
        graph: &mut WeightedGraph,
        mut args: impl Iterator<Item = &'args str>,
    ) -> CmdResult {
        if let Some(path_str) = args.next() {
            let path = Path::new(path_str);
            let mem = WeightedGraph::save_to_memory(&graph);
            std::fs::write(path, mem)?;
            console_log!(cout, Info, "graph saved to \"{path_str}\"");
            Ok(CmdMessage::None)
        } else {
            Err(CmdError::CheckUsage(Cmd::SvLoad))
        }
    }

    /// Returns true if the program should close
    pub fn run(
        cout: &mut ConsoleOut,
        cin: &mut ConsoleIn,
        graph: &mut WeightedGraph,
        route: &mut Option<RouteGenerator>,
        is_debugging: &mut bool,
        is_giving_interactive_targets: &mut bool,
        interactive_targets: &mut Vec<VertexID>,
        camera: &mut Camera3D,
        tempo: &mut Tempo,
    ) -> bool {
        if let Some(cmd_line) = cin.submit_cmd(cout).map(|s| s.to_string()) {
            cmd_line
                .split('|')
                .scan((false, String::new()), |(is_awaited, prev_ret), cmd_args| {
                    match Cmd::parse(cmd_args) {
                        Some(Ok((mut cmd, args))) => {
                            let result = {
                                let mut args = args.chain(prev_ret.as_str().split_whitespace()).filter(|arg| !arg.is_empty());
                                while matches!(cmd, Cmd::Await) {
                                    *is_awaited = true;
                                    match Cmd::try_from_str(args.next().unwrap_or_default()) {
                                        Ok(x) => cmd = x,
                                        Err(e) => {
                                            console_log!(cout, Error, "{e}");
                                            return None;
                                        }
                                    }
                                }
                                match cmd {
                                    Cmd::Help => {
                                        Cmd::run_help(cout);
                                        Ok(CmdMessage::None)
                                    }
                                    Cmd::Echo => {
                                        let value = args.collect::<Vec<_>>().join(" ");
                                        console_log!(cout, Info, "{value}");
                                        Ok(CmdMessage::Return(value))
                                    }
                                    Cmd::Skip => {
                                        if let Some(n) = args.next().and_then(|n| n.parse().ok()) {
                                            Ok(CmdMessage::Return(args.skip(n).collect::<Vec<&str>>().join(" ")))
                                        } else {
                                            Err(CmdError::CheckUsage(Cmd::Skip))
                                        }
                                    }
                                    Cmd::Take => {
                                        if let Some(n) = args.next().and_then(|n| n.parse().ok()) {
                                            Ok(CmdMessage::Return(args.take(n).collect::<Vec<&str>>().join(" ")))
                                        } else {
                                            Err(CmdError::CheckUsage(Cmd::Take))
                                        }
                                    }
                                    Cmd::Await => unreachable!(),
                                    Cmd::SvRouteList => {
                                        if let Some(route) = route {
                                            let list = route.result()
                                                .into_iter()
                                                .copied()
                                                .map(|id| graph.vert(id).id.as_str())
                                                .collect::<Vec<&str>>();

                                            console_log!(cout, Info, "targets: {}", list.join(", "));
                                            Ok(CmdMessage::Return(list.join(" ")))
                                        } else {
                                            console_log!(cout, Info, "no ongoing route");
                                            Ok(CmdMessage::Return(String::new()))
                                        }
                                    }
                                    Cmd::SvRoute | Cmd::SvRouteAdd => {
                                        let result = match cmd {
                                            Cmd::SvRoute    => Cmd::run_sv_route    (cout, cin, graph, route, std::mem::take(interactive_targets), args),
                                            Cmd::SvRouteAdd => Cmd::run_sv_route_add(cout, cin, graph, route, std::mem::take(interactive_targets), args),
                                            _ => unreachable!(),
                                        };
                                        if matches!(result, Ok(CmdMessage::RequestInteractiveTargets)) {
                                            *is_giving_interactive_targets = true;
                                            cin.unfocus();
                                            Ok(CmdMessage::None)
                                        } else {
                                            *is_giving_interactive_targets = false;
                                            if result.is_ok() && *is_awaited {
                                                let route = route.as_mut().unwrap();
                                                loop {
                                                    route.step(cout, graph);
                                                    if route.is_finished() {
                                                        let s = route.result()
                                                            .into_iter()
                                                            .copied()
                                                            .map(|id| graph.vert(id).id.as_str())
                                                            .collect::<Vec<&str>>()
                                                            .join(" ");

                                                        break Ok(CmdMessage::Return(s));
                                                    }
                                                }
                                            } else {
                                                result
                                            }
                                        }
                                    }
                                    Cmd::SvRouteClear => {
                                        *route = None;
                                        console_log!(cout, Info, "route cleared");
                                        Ok(CmdMessage::None)
                                    },
                                    Cmd::SvNew => Cmd::run_sv_new(cout, graph, route, *camera, args),
                                    Cmd::SvEdge => Cmd::run_sv_edge(cout, graph, route, args),
                                    Cmd::SvLoad => Cmd::run_sv_load(cout, graph, route, args),
                                    Cmd::SvSave => Cmd::run_sv_save(cout, graph, args),
                                    Cmd::Tempo => Cmd::run_tempo(cout, tempo, args),
                                    Cmd::Dbg => {
                                        *is_debugging = !*is_debugging;
                                        console_log!(cout, Info, "debugging is now {}", if *is_debugging { "on" } else { "off" });
                                        Ok(CmdMessage::None)
                                    }
                                    Cmd::Focus => Cmd::run_focus(cout, graph, camera, args),
                                    Cmd::Cls => {
                                        cout.clear_log();
                                        Ok(CmdMessage::None)
                                    }
                                    Cmd::Close => {
                                        Ok(CmdMessage::Exit)
                                    }
                                }
                            };

                            *is_awaited = false;
                            prev_ret.clear();

                            match result {
                                Ok(msg) => match msg {
                                    CmdMessage::Return(s) => prev_ret.push_str(&s),
                                    CmdMessage::None => {}
                                    CmdMessage::Exit => return Some(true),
                                    CmdMessage::RequestInteractiveTargets => unreachable!(),
                                }
                                Err(e) => {
                                    use std::error::Error;
                                    use std::fmt::Write;
                                    let mut indent = 0;
                                    let mut e: &dyn Error = &e;
                                    let mut msg = e.to_string();
                                    while let Some(src) = e.source() {
                                        indent += 1;
                                        write!(msg, "\n{:1$}{src}", "", 2*indent).unwrap();
                                        e = src;
                                    }
                                    console_log!(cout, Error, "{msg}");
                                    return None;
                                }
                            }
                            Some(false)
                        }
                        Some(Err(e)) => {
                            console_log!(cout, Error, "{e}");
                            None
                        }
                        None => None,
                    }
                })
                .any(|x| x)
        } else {
            false
        }
    }
}