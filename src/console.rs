use std::{collections::VecDeque, num::NonZeroU32, sync::{Mutex, MutexGuard}, time::{Duration, Instant}};
use raylib::prelude::*;
use crate::command::{Cmd, FromCmdError};

pub mod enrich;

pub static CIN: Mutex<ConsoleIn> = Mutex::new(ConsoleIn::new());

pub struct Cin<'a> {
    inner: MutexGuard<'a, ConsoleIn>,
}
impl std::ops::Deref for Cin<'_> {
    type Target = ConsoleIn;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl std::ops::DerefMut for Cin<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub fn cin() -> Cin<'static> {
    Cin { inner: CIN.lock().unwrap() }
}

pub static COUT: Mutex<ConsoleOut> = Mutex::new(ConsoleOut::new());

pub struct Cout<'a> {
    inner: MutexGuard<'a, ConsoleOut>,
}
impl std::ops::Deref for Cout<'_> {
    type Target = ConsoleOut;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl std::ops::DerefMut for Cout<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub fn cout() -> Cout<'static> {
    Cout { inner: COUT.lock().unwrap() }
}

macro_rules! console_log {
    ($level:ident, $($args:tt)+) => {
        $crate::console::console_log!($crate::console::ConsoleLineCategory::$level, $($args)+)
    };
    ($level:expr, $first:literal $($args:tt)*) => {
        $crate::console::cout().log(
            $level,
            format_args!($first $($args)*),
        )
    };
}

macro_rules! console_dbg {
    ($level:ident, $depth:expr, $($args:tt)+) => {
        $crate::console::console_dbg!($crate::console::ConsoleLineCategory::$level, $depth, $($args)+)
    };
    ($level:expr, $depth:expr, $($args:tt)+) => {
        $crate::console::cout().dbg(
            $level,
            $depth,
            format_args!($($args)+),
        )
    };
}

pub(crate) use {console_log, console_dbg};

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

pub struct ConsoleIn {
    backspace_pressed: Option<Instant>,
    history: VecDeque<String>,
    history_offset: usize,
    is_focused: bool,
    pub current: String,
}

impl ConsoleIn {
    pub const fn new() -> Self {
        Self {
            backspace_pressed: None,
            history: VecDeque::new(),
            history_offset: 0,
            is_focused: true,
            current: String::new(),
        }
    }

    #[inline]
    pub fn is_focused(&self) -> bool {
        self.is_focused
    }

    #[inline]
    pub fn focus(&mut self) {
        self.is_focused = true;
        self.backspace_pressed = None;
    }

    #[inline]
    pub fn unfocus(&mut self) {
        self.is_focused = false;
    }

    /// Returns true on change
    pub fn update_input(&mut self, rl: &mut RaylibHandle) -> bool {
        if rl.is_key_released(KeyboardKey::KEY_BACKSPACE) {
            self.backspace_pressed = None;
        }

        if self.is_focused {
            let mut is_changed = true;

            if let Some(ch) = rl.get_char_pressed() {
                self.current.push(ch);
            } else if rl.is_key_pressed(KeyboardKey::KEY_BACKSPACE) {
                self.backspace_pressed = Some(Instant::now());
                if rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) || rl.is_key_down(KeyboardKey::KEY_RIGHT_CONTROL) {
                    pop_word(&mut self.current);
                } else {
                    self.current.pop();
                }
            } else if let Some(pressed_time) = &mut self.backspace_pressed {
                const DELAY: Duration = Duration::from_millis(550);
                const REP: Duration = Duration::from_millis(33);
                if pressed_time.elapsed() >= DELAY {
                    *pressed_time = Instant::now() - DELAY + REP;
                    if rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) || rl.is_key_down(KeyboardKey::KEY_RIGHT_CONTROL) {
                        pop_word(&mut self.current);
                    } else {
                        self.current.pop();
                    }
                }
            } else if rl.is_key_pressed(KeyboardKey::KEY_UP) {
                self.history_offset = if self.history_offset + 1 < self.history.len() + 1 { self.history_offset + 1 } else { 0 };
                self.current = self.history.get(self.history_offset).cloned().unwrap_or_default();
            } else if rl.is_key_pressed(KeyboardKey::KEY_DOWN) {
                self.history_offset = self.history_offset.checked_sub(1).unwrap_or(self.history.len() + 1 - 1);
                self.current = self.history.get(self.history_offset).cloned().unwrap_or_default();
            } else if rl.is_key_pressed(KeyboardKey::KEY_V) && (rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) || rl.is_key_down(KeyboardKey::KEY_RIGHT_CONTROL)) {
                if let Ok(clipboard) = rl.get_clipboard_text() {
                    self.current.push_str(&clipboard);
                }
            } else {
                is_changed = false;
            }
            is_changed
        } else { false }
    }

    pub fn submit_cmd(&mut self) -> Option<Result<(Cmd, std::str::Split<'_, char>), FromCmdError>> {
        (!self.current.is_empty())
            .then(|| {
                let msg = std::mem::take(&mut self.current);
                self.history.push_front(msg.clone());
                self.history_offset = self.history.len();
                console_log!(Ghost, "{msg}");
                self.history.front()
                    .expect("should have at least one element after push")
                    .split(' ')
            })
            .and_then(|mut args|
                args.next()
                    .map(|first| (first, args))
            )
            .map(|(cmd_str, args)|
                Cmd::try_from_str(cmd_str)
                    .map(|cmd| (cmd, args))
            )
    }
}

pub struct ConsoleOut {
    log: VecDeque<String>,
    dbg: Vec<String>,
}

impl ConsoleOut {
    pub const fn new() -> Self {
        Self {
            log: VecDeque::new(),
            dbg: Vec::new(),
        }
    }

    #[inline]
    pub fn clear_log(&mut self) {
        self.log.clear();
    }

    pub fn log(&mut self, cat: ConsoleLineCategory, msg: std::fmt::Arguments<'_>) {
        let (Color { r, g, b, a }, pre) = cat.color_prefix();
        if self.log.len() == 512 {
            _ = self.log.pop_front();
        }
        self.log.push_back(format!("<color=rgba({r},{g},{b},{a})>{pre}{msg}</color>"));
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

pub fn pop_word(s: &mut String) {
    let st = s.trim_end();
    let new_len = if let Some(last_char) = st.chars().last() {
        if last_char.is_alphanumeric() || last_char == '_' {
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
        }
    } else { 0 };
    s.truncate(new_len);
}
