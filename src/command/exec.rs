use std::{num::NonZeroU32, path::Path, str::FromStr};
use raylib::prelude::*;
use crate::{console::{input::ConsoleIn, output::ConsoleOut, parse_color::RichColor}, console_log, graph::{Adjacent, VertexID, WeightedGraph}, route::RouteGenerator, serialization::LoadGraphError, CommandData, CAMERA_POSITION_DEFAULT, VERTEX_RADIUS};

use super::{Cmd, FromCmdError, Tempo};

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
pub enum CmdError {
    CheckUsage(Cmd),
    VertexDNE(String),
    MissingArgs(&'static str),
    NoExistingRoute,
    ParseCoordsFailed(ParseCoordsError),
    ParseTempoTicksFailed(std::num::ParseIntError),
    ParseTempoMillisecondsFailed(std::num::ParseIntError),
    LoadGraphFailed(LoadGraphError),
    FromCmdError(FromCmdError),
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
            Self::FromCmdError(_) => f.write_str("no such command exists"),
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

            Self::FromCmdError(e) => Some(e),
            Self::IOError(e) => Some(e),
        }
    }
}
impl From<std::io::Error> for CmdError {
    fn from(value: std::io::Error) -> Self {
        CmdError::IOError(value)
    }
}

pub type CmdResult = Result<String, CmdError>;

impl Cmd {
    fn parse(s: &str) -> Option<Result<(Cmd, std::str::SplitWhitespace<'_>), FromCmdError>> {
        let mut args = s.split_whitespace();
        args.next()
            .map(|first|
                Cmd::try_from_str(first)
                    .map(|cmd| (cmd, args))
            )
    }

    pub fn predict(s: &str) -> ! {
        todo!()
        // debug_assert!(!s.is_empty(), "prediction request should not be empty");
        // let current_line = s.rsplit_once('|').map_or(s, |(_, s)| s).split_whitespace().peekable();
        // current_line.scan(, |s| s.)
        // let num_args = current_line.clone().count();
        // if num_args != 0 {
        //     let first_arg = current_line.next();
        //     if num_args == 1 {
        //         Cmd::LIST.iter()
        //             .copied()
        //             .filter_map(|cmd| {
        //                 cmd.input()
        //                     .strip_prefix(&*s)
        //                     .map(|rem| (cmd, rem))
        //             })
        //     } else {

        //     }
        // } else {
        //     None
        // }
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
                Ok(String::new())
            } else {
                let vert = graph.verts().iter()
                    .find(|vert| (vert.id.eq_ignore_ascii_case(target) || vert.alias.eq_ignore_ascii_case(target)));

                if let Some(vert) = vert {
                    camera.target = vert.pos;
                    camera.position = camera.target + Vector3::new(0.0, 400.0, 0.0);
                    Ok(String::new())
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
            Ok(id.to_string())
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
        is_giving_interactive_targets: &mut bool,
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
                *is_giving_interactive_targets = true;
                cin.unfocus();
                return Ok(String::new());
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
        Ok(String::new())
    }

    pub fn run_sv_route_add<'args>(
        cout: &mut ConsoleOut,
        cin: &mut ConsoleIn,
        graph: &WeightedGraph,
        route: &mut Option<RouteGenerator>,
        is_giving_interactive_targets: &mut bool,
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
                *is_giving_interactive_targets = true;
                return Ok(String::new());
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
        Ok(String::new())
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
        Ok(String::new())
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
        Ok(String::new())
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
        Ok(String::new())
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
            Ok(String::new())
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
            Ok(String::new())
        } else {
            Err(CmdError::CheckUsage(Cmd::SvLoad))
        }
    }

    fn run_once<'args>(
        mut self,
        mut args: std::iter::Peekable<impl Iterator<Item = &'args str>>,
        cout: &mut ConsoleOut,
        cin: &mut ConsoleIn,
        data: &mut CommandData,
    ) -> CmdResult {
        let is_awaited = matches!(self, Cmd::Await);
        while matches!(self, Cmd::Await) {
            match Cmd::try_from_str(args.next().unwrap_or_default()) {
                Ok(x) => self = x,
                Err(e) => return Err(CmdError::FromCmdError(e)),
            }
        }
        match self {
            Cmd::Help => {
                Cmd::run_help(cout);
                Ok(String::new())
            }
            Cmd::Echo => {
                let value = args.collect::<Vec<_>>().join(" ");
                console_log!(cout, Info, "{value}");
                Ok(value)
            }
            Cmd::Skip => {
                if let Some(n) = args.next().and_then(|n| n.parse().ok()) {
                    Ok(args.skip(n).collect::<Vec<&str>>().join(" "))
                } else {
                    Err(CmdError::CheckUsage(Cmd::Skip))
                }
            }
            Cmd::Take => {
                if let Some(n) = args.next().and_then(|n| n.parse().ok()) {
                    Ok(args.take(n).collect::<Vec<&str>>().join(" "))
                } else {
                    Err(CmdError::CheckUsage(Cmd::Take))
                }
            }
            Cmd::Await => unreachable!(),
            Cmd::SvRouteList => {
                if let Some(route) = &mut data.route {
                    let list = route.result()
                        .into_iter()
                        .copied()
                        .map(|id| data.graph.vert(id).id.as_str())
                        .collect::<Vec<&str>>();

                    console_log!(cout, Info, "targets: {}", list.join(", "));
                    Ok(list.join(" "))
                } else {
                    console_log!(cout, Info, "no ongoing route");
                    Ok(String::new())
                }
            }
            Cmd::SvRoute | Cmd::SvRouteAdd => {
                let result = match self {
                    Cmd::SvRoute    => Cmd::run_sv_route    (cout, cin, &mut data.graph, &mut data.route, &mut data.is_giving_interactive_targets, std::mem::take(&mut data.interactive_targets), args),
                    Cmd::SvRouteAdd => Cmd::run_sv_route_add(cout, cin, &mut data.graph, &mut data.route, &mut data.is_giving_interactive_targets, std::mem::take(&mut data.interactive_targets), args),
                    _ => unreachable!(),
                };
                if result.is_ok() && data.is_giving_interactive_targets {
                    Ok(String::new())
                } else {
                    data.is_giving_interactive_targets = false;
                    if result.is_ok() && is_awaited {
                        let route = data.route.as_mut().unwrap();
                        loop {
                            route.step(cout, &mut data.graph);
                            if route.is_finished() {
                                let s = route.result()
                                    .into_iter()
                                    .copied()
                                    .map(|id| data.graph.vert(id).id.as_str())
                                    .collect::<Vec<&str>>()
                                    .join(" ");

                                break Ok(s);
                            }
                        }
                    } else {
                        result
                    }
                }
            }
            Cmd::SvRouteClear => {
                data.route = None;
                console_log!(cout, Info, "route cleared");
                Ok(String::new())
            },
            Cmd::SvNew => Cmd::run_sv_new(cout, &mut data.graph, &mut data.route, data.camera, args),
            Cmd::SvEdge => Cmd::run_sv_edge(cout, &mut data.graph, &mut data.route, args),
            Cmd::SvLoad => Cmd::run_sv_load(cout, &mut data.graph, &mut data.route, args),
            Cmd::SvSave => Cmd::run_sv_save(cout, &mut data.graph, args),
            Cmd::Tempo => Cmd::run_tempo(cout, &mut data.tempo, args),
            Cmd::Dbg => {
                data.is_debugging = !data.is_debugging;
                console_log!(cout, Info, "debugging is now {}", if data.is_debugging { "on" } else { "off" });
                Ok(String::new())
            }
            Cmd::Focus => Cmd::run_focus(cout, &mut data.graph, &mut data.camera, args),
            Cmd::Cls => {
                cout.clear_log();
                Ok(String::new())
            }
            Cmd::Close => {
                data.should_close = true;
                Ok(String::new())
            }
            Cmd::ColorVerts | Cmd::ColorEdges | Cmd::ColorBackground => {
                let color: Option<Color> = args.next()
                    .and_then(|x| RichColor::from_str(x).ok())
                    .and_then(|RichColor(color)|
                        args.next()
                            .map_or(
                                Some(color),
                                |opacity_str| opacity_str
                                    .strip_suffix('%')
                                    .and_then(|x| x.parse::<f32>().ok())
                                    .map(|a| color.alpha(a*0.01))
                            )
                    );
                if let Some(color) = color {
                    *match self {
                        Cmd::ColorVerts => &mut data.verts_color,
                        Cmd::ColorEdges => &mut data.edges_color,
                        Cmd::ColorBackground => &mut data.background_color,
                        _ => unreachable!(),
                    } = color;
                    Ok(String::new())
                } else {
                    Err(CmdError::CheckUsage(self))
                }
            }
        }
    }

    /// Returns true if the program should close
    pub fn run(cout: &mut ConsoleOut, cin: &mut ConsoleIn, data: &mut CommandData) -> bool {
        if let Some(cmd_line) = cin.submit_cmd(cout).map(|s| s.to_string()) {
            cmd_line
                .split('|')
                .scan(String::new(), |prev_ret, cmd_args| {
                    match Cmd::parse(cmd_args) {
                        Some(Ok((cmd, args))) => {
                            let result = cmd.run_once(
                                args.chain(prev_ret.as_str().split_whitespace()).filter(|arg| !arg.is_empty()).peekable(),
                                cout,
                                cin,
                                data,
                            );

                            prev_ret.clear();

                            match result {
                                Ok(msg) => prev_ret.push_str(&msg),
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