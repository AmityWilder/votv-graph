use std::sync::{Mutex, MutexGuard};
use input::ConsoleIn;
use output::ConsoleOut;

pub mod enrich;
pub mod parse_color;
pub mod input;
pub mod output;

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
        $crate::console::console_log!($crate::console::output::ConsoleLineCategory::$level, $($args)+)
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
        $crate::console::console_dbg!($crate::console::output::ConsoleLineCategory::$level, $depth, $($args)+)
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
