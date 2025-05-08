use std::{borrow::Cow, collections::VecDeque, num::NonZeroU32, str::FromStr};
// use exec::{CmdError, CmdResult};
use raylib::prelude::* ;
use snippet::Snippet;
use crate::{console::{input::ConsoleIn, output::ConsoleOut}, console_log, graph::{VertexID, WeightedGraph}, route::RouteGenerator};

pub mod snippet;
// pub mod exec;
mod cmd;

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

#[derive(Debug)]
pub enum Cmd {
    Help,
    Close,
    Cls,
    Echo,
    Dbg,
    Focus,
    ColorVerts,
    ColorEdges,
    ColorBackground,
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

impl Cmd {

}

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
