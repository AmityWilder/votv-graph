use std::{cell::RefCell, collections::VecDeque, sync::{LazyLock, LockResult, ReentrantLock, ReentrantLockGuard, RwLock, RwLockReadGuard, RwLockWriteGuard}};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Category {
    Route,
    Ghost,
    Command,
    #[allow(dead_code, reason = "for future use")]
    Debug,
    Info,
    Warning,
    Error,
    #[allow(dead_code, reason = "for future use")]
    Fatal,
}

#[derive(Debug, Clone)]
pub struct ConsoleLine {
    pub cat: Category,
    pub msg: String,
}

impl ConsoleLine {
    pub fn as_line_ref(&self) -> ConsoleLineRef<'_> {
        ConsoleLineRef::new(self.cat, &self.msg)
    }
}

#[derive(Debug, Clone)]
pub struct ConsoleLineRef<'a> {
    pub cat: Category,
    pub msg: &'a str,
}

impl<'a> ConsoleLineRef<'a> {
    pub fn new(cat: Category, msg: &'a str) -> Self {
        Self {
            cat,
            msg,
        }
    }

    pub fn ghost(msg: &'a str) -> Self {
        Self::new(Category::Ghost, msg)
    }
    pub fn command(msg: &'a str) -> Self {
        Self::new(Category::Command, msg)
    }
}

enum CinIter<'a> {
    Empty(bool),
    NonEmpty {
        iter: std::collections::vec_deque::Iter<'a, String>,
        n: usize,
    },
}
impl<'a> CinIter<'a> {
    fn new(collection: &'a VecDeque<String>, history: usize) -> Self {
        if collection.is_empty() {
            Self::Empty(true)
        } else {
            Self::NonEmpty {
                iter: collection.iter(),
                n: history + 1,
            }
        }
    }
}
impl<'a> Iterator for CinIter<'a> {
    type Item = ConsoleLineRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            CinIter::Empty(item) => {
                std::mem::take(item)
                    .then(|| ConsoleLineRef::command(""))
            }
            CinIter::NonEmpty { iter, n } => {
                if *n >= 1 {
                    *n -= 1;
                    iter.nth_back(iter.len().saturating_sub(*n))
                        .map(|msg| ConsoleLineRef {
                            cat: if *n == 0 { Category::Command } else { Category::Ghost },
                            msg: &msg,
                        })
                } else {
                    None
                }
            }
        }
    }
}

struct ConsoleIn {
    /// push to front \
    /// index 0 is always the current command
    buf: VecDeque<String>,
    offset: usize,
}
impl ConsoleIn {
    fn new() -> Self {
        Self {
            buf: VecDeque::new(),
            offset: 0,
        }
    }

    fn push_char(&mut self, ch: char) {
        if self.buf.is_empty() {
            self.buf.push_front(String::new());
        }
        self.buf.front_mut()
            .expect("should have at least one element after pushing")
            .push(ch);
    }

    fn push_str(&mut self, string: &str) {
        if self.buf.is_empty() {
            self.buf.push_front(String::new());
        }
        self.buf.front_mut()
            .expect("should have at least one element after pushing")
            .push_str(string);
    }

    fn pull(&self) -> &str {
        self.buf.front().map_or("", String::as_str)
    }

    pub fn iter(&self, history: usize) -> impl Iterator<Item = ConsoleLineRef<'_>> {
        CinIter::new(&self.buf, history)
    }
}

struct ConsoleOut {
    buf: Vec<ConsoleLine>,
}
impl ConsoleOut {
    fn new() -> Self {
        Self {
            buf: Vec::new(),
        }
    }

    fn push_line(&mut self, cat: Category, msg: &str) {
        self.buf.push(ConsoleLine { cat, msg: msg.to_string() });
    }

    fn iter(&self) -> impl ExactSizeIterator<Item = ConsoleLineRef<'_>> + DoubleEndedIterator {
        self.buf.iter()
            .map(ConsoleLine::as_line_ref)
    }
}

struct ConsoleDebug {
    buf: Vec<ConsoleLine>,
}
impl ConsoleDebug {
    fn new() -> Self {
        Self {
            buf: Vec::new(),
        }
    }

    fn push_line(&mut self, depth: usize, cat: Category, msg: &str) {
        self.buf.truncate(depth);
        if self.buf.len() == depth {
            self.buf.push(ConsoleLine { cat, msg: msg.to_string() });
        } else {
            panic!("cannot push more than one depth (current depth: {}, write depth: {})\nexisting content: {:?}\nwanted to print: \"{:?}\"",
                self.buf.len() as isize - 1,
                depth,
                &self.buf,
                msg,
            );
        }
    }
}

static CIN:  LazyLock<ReentrantLock<RefCell<ConsoleIn>>>    = LazyLock::new(|| ReentrantLock::new(RefCell::new(ConsoleIn   ::new())));
static COUT: LazyLock<ReentrantLock<RefCell<ConsoleOut>>>   = LazyLock::new(|| ReentrantLock::new(RefCell::new(ConsoleOut  ::new())));
static CDBG: LazyLock<ReentrantLock<RefCell<ConsoleDebug>>> = LazyLock::new(|| ReentrantLock::new(RefCell::new(ConsoleDebug::new())));

pub fn cin() -> ReentrantLockGuard<'static, RefCell<ConsoleIn>> {
    CIN.lock()
}
pub fn cout() -> ReentrantLockGuard<'static, RefCell<ConsoleOut>> {
    COUT.lock()
}
pub fn cdbg() -> ReentrantLockGuard<'static, RefCell<ConsoleDebug>> {
    CDBG.lock()
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
