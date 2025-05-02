use std::{borrow::Cow, num::NonZeroU32, sync::RwLock};
use raylib::prelude::*;

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
    Trace,
    Debug,
    Info,
    Warning,
    Error,
    Fatal,
}

#[derive(Debug)]
pub struct InvalidLogLevelError(TraceLogLevel);

impl std::fmt::Display for InvalidLogLevelError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "the TraceLogLevel `{:?}` is not intended for logging", self.0)
    }
}

impl std::error::Error for InvalidLogLevelError {}

impl TryFrom<TraceLogLevel> for ConsoleLineCategory {
    type Error = InvalidLogLevelError;

    fn try_from(value: TraceLogLevel) -> Result<Self, InvalidLogLevelError> {
        match value {
            TraceLogLevel::LOG_TRACE   => Ok(Self::Trace),
            TraceLogLevel::LOG_DEBUG   => Ok(Self::Debug),
            TraceLogLevel::LOG_INFO    => Ok(Self::Info),
            TraceLogLevel::LOG_WARNING => Ok(Self::Warning),
            TraceLogLevel::LOG_ERROR   => Ok(Self::Error),
            TraceLogLevel::LOG_FATAL   => Ok(Self::Fatal),

            | TraceLogLevel::LOG_ALL
            | TraceLogLevel::LOG_NONE
                => Err(InvalidLogLevelError(value)),
        }
    }
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

    pub fn ghost(msg: &'a str) -> Self {
        Self::new(ConsoleLineCategory::Ghost, msg)
    }
    pub fn command(msg: &'a str) -> Self {
        Self::new(ConsoleLineCategory::Command, msg)
    }
}

pub struct Console {
    pub command: String,
    pub reply: Vec<ConsoleLine>,
    pub debug: Vec<ConsoleLine>,
}

impl Console {
    pub const fn new() -> Self {
        Self {
            command: String::new(),
            reply: Vec::new(),
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

pub static CONSOLE: RwLock<Console> = RwLock::new(Console::new());

macro_rules! console_read {
    () => { $crate::console::CONSOLE.read().unwrap() };
}
macro_rules! console_write {
    () => { $crate::console::CONSOLE.write().unwrap() };
}

macro_rules! console_log {
    ($level:ident, $($args:tt)+) => {
        $crate::console::CONSOLE.write().unwrap().reply(
            $crate::console::ConsoleLineCategory::$level,
            format_args!($($args)+),
        )
    };
    ($level:expr, $($args:tt)+) => {
        $crate::console::CONSOLE.write().unwrap().reply(
            $level,
            format_args!($($args)+),
        )
    };
}

macro_rules! console_dbg {
    ($level:ident, $depth:expr, $($args:tt)+) => {
        $crate::console::CONSOLE.write().unwrap().debug(
            $crate::console::ConsoleLineCategory::$level,
            $depth,
            format_args!($($args)+),
        )
    };
    ($level:expr, $depth:expr, $($args:tt)+) => {
        $crate::console::CONSOLE.write().unwrap().debug(
            $level,
            $depth,
            format_args!($($args)+),
        )
    };
}

pub(crate) use {console_read, console_write, console_log, console_dbg};
