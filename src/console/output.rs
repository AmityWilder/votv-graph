use std::collections::VecDeque;
use raylib::prelude::*;

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

impl ConsoleLineCategory {
    pub const fn color_prefix(self) -> (Color, &'static str) {
        let (mut c, pre) = match self {
            Self::Route   => (Color::RAYWHITE,  "route: "  ),
            Self::Ghost | Self::Command => (Color::LIGHTBLUE, ">"),
            Self::Trace   => (Color::DARKGRAY,  "trace: "  ),
            Self::Debug   => (Color::MAGENTA,   "debug: "  ),
            Self::Info    => (Color::LIGHTGRAY, ""         ),
            Self::Warning => (Color::GOLD,      "warning: "),
            Self::Error   => (Color::RED,       "err: "    ),
            Self::Fatal   => (Color::SALMON,    "fatal: "  ),
        };
        if matches!(self, Self::Ghost) { c.a /= 2; }
        (c, pre)
    }
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

pub struct ConsoleOut {
    log: VecDeque<String>,
    dbg: Vec<String>,
    is_dirty: bool,
}

impl ConsoleOut {
    pub const fn new() -> Self {
        Self {
            log: VecDeque::new(),
            dbg: Vec::new(),
            is_dirty: true,
        }
    }

    #[inline]
    pub const fn is_dirty(&self) -> bool {
        self.is_dirty
    }

    #[inline]
    pub const fn mark_clean(&mut self) {
        self.is_dirty = false;
    }

    #[inline]
    pub fn clear_log(&mut self) {
        self.log.clear();
        self.is_dirty = true;
    }

    pub fn log(&mut self, cat: ConsoleLineCategory, msg: std::fmt::Arguments<'_>) {
        let (Color { r, g, b, a }, pre) = cat.color_prefix();
        if self.log.len() == 512 {
            _ = self.log.pop_front();
        }
        self.log.push_back(format!("<color=rgba({r},{g},{b},{a})>{pre}{msg}</color>"));
        self.is_dirty = true;
    }

    pub fn dbg(&mut self, cat: ConsoleLineCategory, depth: usize, msg: std::fmt::Arguments<'_>) {
        self.dbg.truncate(depth);

        assert_eq!(self.dbg.len(), depth,
            "cannot push more than one depth (current depth: {}, write depth: {})\nconsole: {:?}\nwanted to print: \"{:?}\"",
            self.dbg.len() as isize - 1,
            depth,
            &self.dbg,
            msg,
        );

        let (Color { r, g, b, a }, pre) = cat.color_prefix();
        self.dbg.push(format!("<color=rgba({r},{g},{b},{a})>{pre}{msg}</color>"));
        self.is_dirty = true;
    }

    #[inline]
    pub fn log_history(&self) -> impl DoubleEndedIterator<Item = &'_ str> + ExactSizeIterator + Clone {
        self.log.iter().map(String::as_str)
    }

    #[inline]
    pub fn dbg_history(&self) -> impl DoubleEndedIterator<Item = &'_ str> + ExactSizeIterator + Clone {
        self.dbg.iter().map(String::as_str)
    }
}