use crate::{command::{Command, ProgramData, Usage}, console::{input::ConsoleIn, output::ConsoleOut}};

pub enum ClsCmd {
    A(ClsCmdA)
}

impl Command for ClsCmd {
    const INPUT: &str = "cls";
    const DESCRIPTION: &str = "Clear the console.";
    const USAGES: &[Self] = &[Self::A(ClsCmdA {})];
}

pub struct ClsCmdA {}

impl Usage for ClsCmdA {
    type Args<'a> = ();
    type Ret<'a> = ();
    type Err = !;

    const DESCRIPTION: &str = ClsCmd::DESCRIPTION;

    fn run<'a>(cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, _args: Self::Args<'a>) -> Result<Self::Ret<'a>, Self::Err> {
        cout.clear_log();
        Ok(())
    }
}
