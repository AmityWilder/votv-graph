use std::{borrow::Cow, num::NonZeroU32, path::Path};
use raylib::prelude::*;
use crate::{graph::{Adjacent, VertexID, WeightedGraph}, route::RouteGenerator, serialization::LoadGraphError, CAMERA_POSITION_DEFAULT, VERTEX_RADIUS};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConsoleLineCategory {
    Route,
    Ghost,
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
    pub cat: ConsoleLineCategory,
    pub msg: String,
}

impl ConsoleLine {
    pub fn as_line_ref(&self) -> ConsoleLineRef<'_> {
        ConsoleLineRef::new(self.cat, &self.msg)
    }
}

#[derive(Debug, Clone)]
pub struct ConsoleLineRef<'a> {
    pub cat: ConsoleLineCategory,
    pub msg: Cow<'a, str>,
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
    pub fn ghost(msg: &'a str) -> Self {
        Self::new(ConsoleLineCategory::Ghost, msg)
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

macro_rules! console_write {
    ($cons:expr, $level:ident, $($args:tt)+) => {
        $cons.reply(
            $crate::console::ConsoleLineCategory::$level,
            format_args!($($args)+),
        )
    };
}
macro_rules! console_debug {
    ($cons:expr, $level:ident, $depth:expr, $($args:tt)+) => {
        $cons.debug(
            $crate::console::ConsoleLineCategory::$level,
            $depth,
            format_args!($($args)+),
        )
    };
}
pub(crate) use {console_write, console_debug};

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
            pub fn try_from_str(s: &str) -> Result<Self, &str> {
                match s {
                    $($input => Ok(Self::$Variant),)+
                    _ => Err(s),
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
                let usage = cmd.args();
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

pub type CmdResult<T> = Result<T, CmdError>;

impl Cmd {
    pub fn run_focus(
        graph: &WeightedGraph,
        camera: &mut Camera3D,
        console: &mut Console,
        mut args: std::str::Split<'_, char>,
    ) -> CmdResult<()> {
        if let Some(target) = args.next() {
            if target == "reset" {
                camera.position = CAMERA_POSITION_DEFAULT;
                camera.target = Vector3::zero();
                Ok(())
            } else {
                let vert = graph.verts().iter()
                    .find(|vert| (vert.id.eq_ignore_ascii_case(target) || vert.alias.eq_ignore_ascii_case(target)));

                if let Some(vert) = vert {
                    camera.target = vert.pos;
                    camera.position = camera.target + Vector3::new(0.0, 400.0, 0.0);
                    Ok(())
                } else {
                    Err(CmdError::VertexDNE(target.to_string()))
                }
            }
        } else {
            let vert = graph.verts().iter()
                .find(|vert| check_collision_spheres(vert.pos, VERTEX_RADIUS, camera.target, 1.0));

            if let Some(target) = vert {
                console_write!(console, Info, "currently focusing vertex {}", target.id);
            } else {
                console_write!(console, Info, "no vertex is currently focused");
            }
            Ok(())
        }
    }

    pub fn run_help(console: &mut Console) {
        let usages = Cmd::LIST.into_iter().copied()
            .flat_map(|item|
                item.args().into_iter().copied()
                    .map(move |args| (item.input(), args))
            )
            .map(|(input, args)| format!("<color=rgb(0, 150, 230)>{input}</color><color=rgb(0, 120, 185)>{args}</color>"))
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

        let msg = std::iter::once("<color=rgb(72, 241, 157)>Commands:</color>")
            .chain(lines.iter().map(String::as_str))
            .collect::<Vec<_>>()
            .join("\n    ");

        console_write!(console, Info, "{msg}");
    }

    pub fn run_sv_route(
        graph: &WeightedGraph,
        route: &mut Option<RouteGenerator>,
        interactive_targets: Vec<VertexID>,
        console: &mut Console,
        args: std::str::Split<'_, char>,
    ) -> CmdResult<bool> {
        let mut args = args.peekable();
        let targets = match args.peek() {
            Some(&("-i" | "interactive")) => {
                console_write!(console, Info, "click each target with the mouse; order doesn't matter except that the first will be the start");
                console_write!(console, Info, "click a target again to un-target it");
                console_write!(console, Info, "run the command `<color=rgb(0, 150, 230)>sv.route</color>` (without arguments) when finished");
                console.command.push_str("sv.route");
                return Ok(false);
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
        console_write!(console, Info, "generating route");
        Ok(true)
    }

    pub fn run_sv_route_add(
        graph: &WeightedGraph,
        route: &mut Option<RouteGenerator>,
        interactive_targets: Vec<VertexID>,
        console: &mut Console,
        args: std::str::Split<'_, char>,
    ) -> CmdResult<bool> {
        let route = route.as_mut().ok_or(CmdError::NoExistingRoute)?;
        let mut args = args.peekable();
        let targets = match args.peek() {
            Some(&("-i" | "interactive")) => {
                console_write!(console, Info, "click each target with the mouse; order doesn't matter");
                console_write!(console, Info, "click a target again to un-target it");
                console_write!(console, Info, "run the command `<color=rgb(0, 150, 230)>sv.route.add</color>` (without arguments) when finished");
                console.command.push_str("sv.route.add");
                return Ok(false);
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
        route.add_targets(console, targets);
        console_write!(console, Info, "extending route");
        Ok(true)
    }

    pub fn run_sv_new(
        graph: &mut WeightedGraph,
        route: &mut Option<RouteGenerator>,
        camera: Camera3D,
        console: &mut Console,
        mut args: std::str::Split<'_, char>,
    ) -> CmdResult<()> {
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
        console_write!(console, Info, "created new vertex");
        Ok(())
    }

    pub fn run_sv_edge(
        graph: &mut WeightedGraph,
        route: &mut Option<RouteGenerator>,
        console: &mut Console,
        mut args: std::str::Split<'_, char>,
    ) -> CmdResult<()> {
        let a = args.next().ok_or(CmdError::CheckUsage(Cmd::SvEdge))?;
        let b = args.next().ok_or(CmdError::CheckUsage(Cmd::SvEdge))?;
        let a = graph.find_vert(a).map_err(|id| CmdError::VertexDNE(id.to_string()))?;
        let b = graph.find_vert(b).map_err(|id| CmdError::VertexDNE(id.to_string()))?;
        if graph.adjacent(a).iter().any(|Adjacent { vertex, .. }| vertex == &b) {
            console_write!(console, Warning, "vertices {a} and {b} are already connected");
        } else {
            graph.add_edge(a, b);
            *route = None;
            console_write!(console, Info, "created an edge connecting vertices {a} and {b}");
        }
        Ok(())
    }

    pub fn run_tempo(
        console: &mut Console,
        mut args: std::str::Split<'_, char>,
        tempo: &mut Tempo,
    ) -> CmdResult<()> {
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
            console_write!(console, Info, "set tempo to {tempo}");
        } else {
            console_write!(console, Info, "current tempo is {tempo}");
        }
        Ok(())
    }

    pub fn run_sv_load(
        graph: &mut WeightedGraph,
        route: &mut Option<RouteGenerator>,
        console: &mut Console,
        mut args: std::str::Split<'_, char>,
    ) -> CmdResult<()> {
        if let Some(path_str) = args.next() {
            let path = Path::new(path_str);
            let mem = std::fs::read_to_string(path)?;
            *graph = WeightedGraph::load_from_memory(mem).map_err(|e| CmdError::LoadGraphFailed(e))?;
            *route = None;
            console_write!(console, Info, "graph loaded from \"{path_str}\"");
            Ok(())
        } else {
            Err(CmdError::CheckUsage(Cmd::SvLoad))
        }
    }

    pub fn run_sv_save(
        graph: &mut WeightedGraph,
        console: &mut Console,
        mut args: std::str::Split<'_, char>,
    ) -> CmdResult<()> {
        if let Some(path_str) = args.next() {
            let path = Path::new(path_str);
            let mem = WeightedGraph::save_to_memory(&graph);
            std::fs::write(path, mem)?;
            console_write!(console, Info, "graph saved to \"{path_str}\"");
            Ok(())
        } else {
            Err(CmdError::CheckUsage(Cmd::SvLoad))
        }
    }
}

pub struct Enrich<'a> {
    line: &'a str,
    char_width: f32,
    spacing: f32,
    color: Color,
    x: f32,
    upcoming: Option<<Self as Iterator>::Item>,
}
impl<'a> Enrich<'a> {
    pub fn new(line: &'a str, char_width: f32, spacing: f32, color: Color) -> Self {
        Self {
            line,
            char_width,
            spacing,
            color,
            x: 0.0,
            upcoming: None,
        }
    }
}

pub trait EnrichEx: AsRef<str> {
    fn enrich(&self, char_width: f32, spacing: f32, color: Color) -> Enrich<'_> {
        Enrich::new(self.as_ref(), char_width, spacing, color)
    }
}
impl EnrichEx for str {}

impl<'a> Iterator for Enrich<'a> {
    type Item = (f32, &'a str, Color);

    fn next(&mut self) -> Option<Self::Item> {
        if self.upcoming.is_some() {
            self.upcoming.take()
        } else {
            let mut pre;
            let mut rest = self.line;
            while let Some(color_start) = rest.find("<color=rgb(") {
                (pre, rest) = rest.split_at(color_start);
                if let Some(element_len) = rest.find(")>") {
                    let element = &rest[..element_len];
                    let rest = &rest[element_len + ")>".len()..];
                    let color_str = &element["<color=rgb(".len()..];
                    if color_str.len() <= "255, 255, 255".len() {
                        let mut color_iter = color_str.split(',').flat_map(|s| s.trim().parse::<u8>());
                        let r = color_iter.next();
                        let g = color_iter.next();
                        let b = color_iter.next();
                        if let (Some(r), Some(g), Some(b)) = (r, g, b) {
                            if let Some(colored_len) = rest.find("</color>") {
                                let colored = &rest[..colored_len];
                                self.line = &rest[colored_len + "</color>".len()..];

                                let item = (self.x, pre, self.color);
                                self.x += self.char_width*pre.len() as f32 + self.spacing*(pre.len() as f32 - 1.0);
                                self.upcoming = Some((self.x, colored, Color::new(r, g, b, 255)));
                                self.x += self.char_width*colored.len() as f32 + self.spacing*(colored.len() as f32 - 1.0);
                                return Some(item);
                            }
                        }
                    }
                }
                rest = &rest[color_start + 1..];
            }
            if !self.line.is_empty() {
                Some((self.x, std::mem::replace(&mut self.line, ""), self.color))
            } else {
                None
            }
        }
    }
}

pub fn pop_word(s: &mut String) {
    let st = s.trim_end();
    if let Some(last_char) = st.chars().last() {
        let new_len = if last_char.is_alphanumeric() || last_char == '_' {
            st.trim_end_matches(|c: char| c.is_alphanumeric() || c == '_').len()
        } else if last_char == ']' {
            let trimmed = st.trim_end_matches(']');
            let len = trimmed.len();
            if trimmed.ends_with('[') { len - 1 } else { len }
        } else if last_char == ')' {
            let trimmed = st.trim_end_matches(')');
            let len = trimmed.len();
            if trimmed.ends_with('{') { len - 1 } else { len }
        } else if last_char == '}' {
            let trimmed = st.trim_end_matches('}');
            let len = trimmed.len();
            if trimmed.ends_with('{') { len - 1 } else { len }
        } else {
            st.trim_end_matches(last_char).len()
        };

        while s.len() > new_len {
            s.pop();
        }
    }
}
