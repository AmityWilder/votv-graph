pub mod enrich;
pub mod input;
pub mod output;

macro_rules! console_log {
    ($cout:expr, $level:ident, $($args:tt)+) => {
        $crate::console::console_log!($cout, $crate::console::output::ConsoleLineCategory::$level, $($args)+)
    };
    ($cout:expr, $level:expr, $first:literal $($args:tt)*) => {
        $cout.log($level, format_args!($first $($args)*))
    };
}

macro_rules! console_dbg {
    ($cout:expr, $level:ident, $depth:expr, $($args:tt)+) => {
        $crate::console::console_dbg!($cout, $crate::console::output::ConsoleLineCategory::$level, $depth, $($args)+)
    };
    ($cout:expr, $level:expr, $depth:expr, $($args:tt)+) => {
        $cout.dbg(
            $level,
            $depth,
            format_args!($($args)+),
        )
    };
}

pub(crate) use {console_log, console_dbg};
