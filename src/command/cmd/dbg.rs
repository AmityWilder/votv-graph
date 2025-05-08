use std::borrow::Cow;
use crate::{command::{Arguments, Command, ProgramData, Return, Usage}, console::{input::ConsoleIn, output::ConsoleOut}};

enum DbgCmd {
    A(DbgCmdA),
    B(DbgCmdB),
}

impl Command for DbgCmd {
    const INPUT: &str = "dbg";
    const DESCRIPTION: &str = "Toggle whether debug messages appear.";
    const USAGES: &[Self] = &[
        Self::A(DbgCmdA {}),
        Self::B(DbgCmdB {}),
    ];
}

pub struct DbgCmdReturn {
    new_state: bool,
}

impl<'a> Return<'a> for DbgCmdReturn {
    type DisplayIter = impl Iterator<Item = Cow<'a, str>>;

    fn convert(self) -> Cow<'a, str> {
        Cow::Borrowed(if self.new_state { "true" } else { "false" })
    }

    fn display(self) -> Self::DisplayIter {
        std::iter::once_with(move || Cow::Owned(format!("Debugging is now {}", if self.new_state { "on" } else { "off" })))
    }
}

pub struct DbgCmdA {}

impl Usage for DbgCmdA {
    type Args<'a> = ();
    type Ret<'a> = DbgCmdReturn;
    type Err = !;

    const DESCRIPTION: &str = "Toggle whether debug messages appear.";

    fn run<'a>(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, _args: Self::Args<'a>) -> Result<Self::Ret<'a>, Self::Err> {
        let new_state = !data.is_debugging;
        data.is_debugging = new_state;
        Ok(DbgCmdReturn { new_state })
    }
}

pub struct DbgCmdB {}

pub struct DbgCmdBArgs {
    new_state: bool,
}

impl Arguments<'_> for DbgCmdBArgs {
    type Err = std::str::ParseBoolError;

    const TEMPLATE: &'static str = "true|false";

    fn parse(_data: &'_ ProgramData, s: &'_ str) -> Result<Self, Self::Err> {
        s.parse::<bool>()
            .map(|new_state| Self { new_state })
    }
}

impl Usage for DbgCmdB {
    type Args<'a> = DbgCmdBArgs;
    type Ret<'a> = DbgCmdReturn;
    type Err = !;

    const DESCRIPTION: &str = "Set whether debug messages appear.";

    fn run<'a>(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, data: &mut ProgramData, DbgCmdBArgs { new_state }: Self::Args<'a>) -> Result<Self::Ret<'a>, Self::Err> {
        data.is_debugging = new_state;
        Ok(DbgCmdReturn { new_state })
    }
}