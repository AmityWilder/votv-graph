use std::borrow::Cow;
use crate::{command::{Arguments, Command, ProgramData, Return, Usage}, console::{input::ConsoleIn, output::ConsoleOut}};

pub enum EchoCmd {
    A(EchoCmdA),
}

impl Command for EchoCmd {
    const INPUT: &str = "echo";
    const DESCRIPTION: &str = "Repeat the arguments";
    const USAGES: &[Self] = &[Self::A(EchoCmdA {})];
}

pub struct EchoArgs<'a> {
    arguments: &'a str,
}

impl<'a> Arguments<'a> for EchoArgs<'a> {
    type Err = !;

    const TEMPLATE: &'static str = "[ANY]...";

    fn parse(_data: &'a ProgramData, arguments: &'a str) -> Result<Self, Self::Err> {
        Ok(Self { arguments })
    }
}

impl<'a> Return<'a> for EchoArgs<'a> {
    type DisplayIter = impl Iterator<Item = Cow<'a, str>>;

    fn convert(self) -> Cow<'a, str> {
        Cow::Borrowed(self.arguments)
    }

    fn display(self) -> Self::DisplayIter {
        std::iter::once_with(move || Cow::Borrowed(self.arguments))
    }
}

pub struct EchoCmdA {}

impl Usage for EchoCmdA {
    type Args<'a> = EchoArgs<'a>;
    type Ret<'a> = EchoArgs<'a>;
    type Err = !;

    const DESCRIPTION: &str = EchoCmd::DESCRIPTION;

    fn run<'a>(_cout: &mut ConsoleOut, _cin: &mut ConsoleIn, _data: &mut ProgramData, args: Self::Args<'a>) -> Result<Self::Ret<'a>, Self::Err> {
        Ok(args)
    }
}