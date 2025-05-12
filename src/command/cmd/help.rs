use std::{borrow::Cow, task::Poll};
use crate::{command::{Cmd, CmdError, Command, ProgramData}, console::{input::ConsoleIn, output::ConsoleOut}, console_log};

pub enum CmdHelp {
    All,
    One(Cmd),
}

impl Command for CmdHelp {
    fn init(_data: &ProgramData, args: &[Cow<'static, str>]) -> Result<Self, CmdError> where Self: Sized {
        if args.is_empty() {
            Ok(Self::All)
        } else if let [cmd_str] = args {
            Ok(Self::One(cmd_str.parse::<Cmd>()?))
        } else {
            Err(CmdError::CheckUsage(Cmd::Help))
        }
    }

    fn step(&mut self, _cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData) -> Poll<Result<Vec<Cow<'static, str>>, CmdError>> {
        todo!()
    }

    fn disp(&self, cout: &mut ConsoleOut, _data: &ProgramData, result: &[Cow<'static, str>]) {
        console_log!(cout, Info, "{}", result[0]);
    }
}
