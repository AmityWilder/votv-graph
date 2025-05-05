use std::{collections::VecDeque, num::NonZeroU32, ops::Range, sync::{Mutex, MutexGuard}, time::{Duration, Instant}};
use raylib::prelude::*;

pub mod enrich;
pub mod parse_color;

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
    Cin { inner: CIN.try_lock().expect("the application is currently single-threaded") }
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
    Cout { inner: COUT.try_lock().expect("the application is currently single-threaded") }
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
    is_dirty: bool,
    current: String,
    /// Where the selection started, measured in `char`s
    selection_head: usize,
    /// Where the selection currently is, measured in `char`s
    selection_tail: usize,
}

impl ConsoleIn {
    pub const fn new() -> Self {
        Self {
            backspace_pressed: None,
            history: VecDeque::new(),
            history_offset: 0,
            is_focused: true,
            is_dirty: true,
            current: String::new(),
            selection_head: 0,
            selection_tail: 0,
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
    const fn minmax_selection(&self) -> (&usize, &usize) {
        if self.selection_head <= self.selection_tail {
            (&self.selection_head, &self.selection_tail)
        } else {
            (&self.selection_tail, &self.selection_head)
        }
    }

    #[inline]
    const fn minmax_selection_mut(&mut self) -> (&mut usize, &mut usize) {
        if self.selection_head <= self.selection_tail {
            (&mut self.selection_head, &mut self.selection_tail)
        } else {
            (&mut self.selection_tail, &mut self.selection_head)
        }
    }

    #[inline]
    pub const fn selection_range(&self) -> Range<usize> {
        range_from_pair(self.selection_head, self.selection_tail)
    }

    #[inline]
    pub const fn _selection_tail(&self) -> usize {
        self.selection_tail
    }

    #[inline]
    pub const fn is_focused(&self) -> bool {
        self.is_focused
    }

    #[inline]
    pub const fn focus(&mut self) {
        self.is_focused = true;
        self.backspace_pressed = None;
    }

    #[inline]
    pub const fn unfocus(&mut self) {
        self.is_focused = false;
    }

    #[inline]
    pub const fn current(&self) -> &str {
        self.current.as_str()
    }

    fn insert_over_selection_internal(current: &mut String, selection_head: &mut usize, selection_tail: &mut usize, is_dirty: &mut bool, string: &str) {
        let selection = range_from_pair(*selection_head, *selection_tail);
        if selection.is_empty() && string.is_empty() { return; }
        let new_cursor = selection.start + string.chars().count();
        current.replace_range(selection, string);
        *selection_head = new_cursor;
        *selection_tail = new_cursor;
        *is_dirty = true;
    }

    pub fn insert_over_selection(&mut self, string: &str) {
        Self::insert_over_selection_internal(
            &mut self.current,
            &mut self.selection_head,
            &mut self.selection_tail,
            &mut self.is_dirty,
            string,
        );
    }

    pub fn insert_char_over_selection(&mut self, ch: char) {
        let mut buf = [b'\0'; 4];
        self.insert_over_selection(ch.encode_utf8(&mut buf));
    }

    pub fn update_input(&mut self, rl: &mut RaylibHandle) {
        if rl.is_key_released(KeyboardKey::KEY_BACKSPACE) {
            self.backspace_pressed = None;
        }

        if self.is_focused {
            let is_ctrl_down = rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) || rl.is_key_down(KeyboardKey::KEY_RIGHT_CONTROL);
            if let Some(ch) = rl.get_char_pressed() {
                self.insert_char_over_selection(ch);
            } else if rl.is_key_pressed(KeyboardKey::KEY_BACKSPACE) {
                self.backspace_pressed = Some(Instant::now());
                let selection_len = self.selection_range().len();
                let pop_len = if selection_len > 0 {
                    selection_len
                } else if is_ctrl_down {
                    self.current.prev_word().chars().count()
                } else { 1 };
                let (min, _) = self.minmax_selection_mut();
                *min = min.saturating_sub(pop_len);
                self.insert_over_selection("");
            } else if let Some(pressed_time) = &mut self.backspace_pressed {
                const DELAY: Duration = Duration::from_millis(550);
                const REP: Duration = Duration::from_millis(33);
                if pressed_time.elapsed() >= DELAY {
                    *pressed_time = Instant::now() - DELAY + REP;
                    let pop_len = if is_ctrl_down {
                        self.current.prev_word().chars().count()
                    } else { 1 };
                    let (min, _) = self.minmax_selection_mut();
                    *min = min.saturating_sub(pop_len);
                    self.insert_over_selection("");
                }
            } else if rl.is_key_pressed(KeyboardKey::KEY_UP) || rl.is_key_pressed(KeyboardKey::KEY_DOWN) {
                self.history_offset = if rl.is_key_pressed(KeyboardKey::KEY_UP) {
                    if self.history_offset + 1 < self.history.len() + 1 { self.history_offset + 1 } else { 0 }
                } else /* KEY_DOWN */ {
                    self.history_offset.checked_sub(1).unwrap_or(self.history.len() + 1 - 1)
                };
                Self::insert_over_selection_internal(
                    &mut self.current,
                    &mut self.selection_head,
                    &mut self.selection_tail,
                    &mut self.is_dirty,
                    self.history.get(self.history_offset)
                        .map(String::as_str)
                        .unwrap_or_default(),
                );
            } else if is_ctrl_down && rl.is_key_pressed(KeyboardKey::KEY_V) {
                if let Ok(clipboard) = rl.get_clipboard_text() {
                    self.insert_over_selection(&clipboard);
                }
            }
        }
    }

    pub fn submit_cmd(&mut self) -> Option<&str> {
        (!self.current.is_empty()).then(|| {
            let msg = std::mem::take(&mut self.current);
            self.history.push_front(msg.clone());
            self.history_offset = self.history.len();
            console_log!(Ghost, "{msg}");
            self.history.front()
                .map(String::as_str)
                .expect("should have at least one element after push")
        })
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

pub trait WordLen {
    type Word;
    fn prev_word(self) -> Self::Word;
    fn next_word(self) -> Self::Word;
}

const BRACKET_PAIRS: [(char, char); 4] = [
    ('[', ']'),
    ('(', ')'),
    ('{', '}'),
    ('<', '>'),
];

fn is_word_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

impl WordLen for &str {
    type Word = Self;

    fn prev_word(self) -> Self::Word {
        let st = self.trim_end();
        if let Some(next_char) = st.chars().next() {
            let trimmed_len = if is_word_char(next_char) {
                st.trim_end_matches(is_word_char).len()
            } else {
                let trimmed = st.trim_end_matches(next_char);
                BRACKET_PAIRS.iter()
                    .copied()
                    .find(|&(a, _)| a == next_char)
                    .and_then(|(_, b)| trimmed.strip_suffix(b))
                    .unwrap_or(trimmed)
                    .len()
            };
            &self[trimmed_len..]
        } else {
            self
        }
    }

    fn next_word(self) -> Self::Word {
        let st = self.trim_start();
        if let Some(next_char) = st.chars().next() {
            let trimmed_len = if is_word_char(next_char) {
                st.trim_start_matches(is_word_char).len()
            } else {
                let trimmed = st.trim_start_matches(next_char);
                BRACKET_PAIRS.iter()
                    .copied()
                    .find(|&(a, _)| a == next_char)
                    .and_then(|(_, b)| trimmed.strip_prefix(b))
                    .unwrap_or(trimmed)
                    .len()
            };
            &self[..self.len() - trimmed_len]
        } else {
            self
        }
    }
}

#[inline]
pub const fn range_from_pair(a: usize, b: usize) -> Range<usize> {
    if a <= b { a..b } else { b..a }
}
