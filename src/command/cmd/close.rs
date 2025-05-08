use std::borrow::Cow;
use crate::{command::{Arguments, Command, ProgramData, Return, Usage, UsageEnum}, console::{input::ConsoleIn, output::ConsoleOut}};

pub enum CloseCmd {}

impl Command for CloseCmd {
    type UsageEnum = CloseCmdEnum;

    const INPUT: &str = "close";
    const DESCRIPTION: &str = "Close the application.";
    const USAGES: &[Self::UsageEnum] = &[CloseCmdEnum::A(CloseCmdA {})];
}

pub enum CloseCmdEnum {
    A,
}

impl UsageEnum for CloseCmdEnum {
    type Cmd = CloseCmd;
    type Ret<'a> = CloseCmdReturn;
    type Err = !;

    fn run<'a>(
        &mut self,
        cout: &mut ConsoleOut,
        cin: &mut ConsoleIn,
        data: &mut ProgramData,
        args: &'a str,
    ) -> Result<Self::Ret<'a>, Self::Err> {
        match self {
            Self::A => {
                let args = <CloseCmdA as Usage>::Args::parse(&*data, args)?;
                cmd.run(cout, cin, data, args)
            }
        }
    }
}

pub struct CloseCmdReturn {}

impl<'a> Return<'a> for CloseCmdReturn {
    type DisplayIter = impl Iterator<Item = Cow<'a, str>>;

    fn convert(self) -> Cow<'a, str> {
        Cow::Borrowed("")
    }

    fn display(self) -> Self::DisplayIter {
        std::iter::once_with(|| Cow::Borrowed("Closing the program..."))
    }
}

pub struct CloseCmdA {}

impl Usage for CloseCmdA {
    type Enum = CloseCmdEnum;
    type Args<'a> = ();
    type Ret<'a> = CloseCmdReturn;
    type Err = !;

    fn run<'a>(
        &mut self,
        _cout: &mut ConsoleOut,
        _cin: &mut ConsoleIn,
        data: &mut ProgramData,
        _args: Self::Args<'a>,
    ) -> Result<Self::Ret<'a>, Self::Err> {
        data.should_close = true;
        Ok(CloseCmdReturn {})
    }
}
