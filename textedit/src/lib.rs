#![feature(char_max_len)]

use std::{clone, collections::VecDeque, num::NonZeroIsize, ops::Range, time::{Duration, Instant}};
pub mod word;
use word::WordsEx;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    Right,
    Left,
    Down,
    Up,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum CursorStep {
    /// column/row
    #[default]
    Character,
    /// word/page
    Segment,
    /// line/document
    Full,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MoveCursor {
    /// In what direction the step is taken
    pub direction: Direction,
    /// Whether the movement should clear the selection and reposition
    /// instead of adding to/removing from the selection
    pub is_selecting: bool,
    /// How far the cursor should move
    pub step_by: CursorStep,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Input {
    Symbol(char),
    MoveCursor(MoveCursor),
    SelectAll,
    Cut,
    Copy,
    Paste,
    /// Set the selection to only the head
    Deselect,
    DeleteBefore,
    DeleteAfter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InputEvent {
    /// No change in input; tick repeater if any inputs are held
    Update,
    Press(Input),
    Release(Input),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HeldInput {
    pub input: Input,
    pub start_time: Instant,
}

impl HeldInput {
    pub fn now(input: Input) -> Self {
        Self { input, start_time: Instant::now() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Repeater {
    /// Time between initial input and first repeat
    pub delay: Duration,

    /// Time between repeats after delay finishes
    pub period: Duration,
}

impl Repeater {
    #[inline]
    pub const fn new(delay: Duration, period: Duration) -> Option<Self> {
        if !period.is_zero() { Some(Self { delay, period }) } else { None }
    }

    #[inline]
    pub const fn count(&self, duration: Duration) -> usize {
        duration.saturating_sub(self.delay)
            .div_duration_f64(self.period) as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Edit {
    pub start: usize,
    pub len: usize,
    pub replacement: String,
}

#[derive(Debug, Clone)]
pub struct Editor {
    /// The most recent input that is still ongoing and when it started
    pub held_input: Option<HeldInput>,

    /// Original cursor position when selection started
    pub selection_head: usize,

    /// Current cursor position
    pub selection_tail: usize,
}

impl Default for Editor {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

pub trait Clipboard {
    fn get(&self) -> Option<&str>;
    fn set(&mut self, s: Option<&str>);
}

impl Editor {
    pub const fn new() -> Self {
        Self {
            held_input: None,
            selection_head: 0,
            selection_tail: 0,
        }
    }

    #[inline]
    pub fn selection_range(&self) -> std::ops::Range<usize> {
        let (head, tail) = (self.selection_head, self.selection_tail);
        if head <= tail { head..tail } else { tail..head }
    }

    fn press<C: Clipboard>(
        &mut self,
        input: Input,
        string: &mut String,
        clipboard: &mut C,
    ) -> Option<Edit> {
        let selection = self.selection_range();

        if matches!(&input, Input::Cut | Input::Copy) {
            clipboard.set(Some(&string[selection.clone()]));
        }

        match &input {
            Input::MoveCursor(m) => {
                self.selection_tail = match (m.step_by, m.direction) {
                    (CursorStep::Full, Direction::Down) => string.len(),
                    (CursorStep::Full, Direction::Up) => 0,
                    _ => {
                        let within = match m.direction {
                            Direction::Right | Direction::Down => &string[self.selection_tail..],
                            Direction::Left  | Direction::Up   => &string[..self.selection_tail],
                        };
                        match m.direction {
                            Direction::Right => match m.step_by {
                                CursorStep::Character => todo!(),
                                CursorStep::Segment => todo!(),
                                CursorStep::Full => todo!(),
                            }
                            Direction::Down => match m.step_by {
                                CursorStep::Character => todo!(),
                                CursorStep::Segment => todo!(),
                                CursorStep::Full => todo!(),
                            }
                            Direction::Left => match m.step_by {
                                CursorStep::Character => todo!(),
                                CursorStep::Segment => todo!(),
                                CursorStep::Full => todo!(),
                            }
                            Direction::Up => match m.step_by {
                                CursorStep::Character => todo!(),
                                CursorStep::Segment => todo!(),
                                CursorStep::Full => todo!(),
                            }
                        }
                        todo!()
                    }
                };

                if !m.is_selecting {
                    self.selection_head = self.selection_tail;
                }
            }

            Input::SelectAll => {
                self.selection_head = 0;
                self.selection_tail = string.len();
            }

            Input::Deselect => self.selection_tail = self.selection_head,

            Input::Copy => {}

            | Input::Symbol(_)
            | Input::Cut
            | Input::Paste
            | Input::DeleteBefore
            | Input::DeleteAfter
                => {
                    let mut buf;
                    let insertion = match input {
                        Input::Symbol(ch) => {
                            buf = [0; char::MAX_LEN_UTF8];
                            ch.encode_utf8(&mut buf)
                        }

                        Input::Paste => clipboard.get().unwrap_or(""),

                        | Input::Cut
                        | Input::DeleteBefore
                        | Input::DeleteAfter
                            => "",

                        _ => unreachable!(),
                    };

                    string.replace_range(selection, insertion);
                    self.selection_tail = self.selection_head;
                }
        }

        None
    }

    pub fn update<C: Clipboard>(
        &mut self,
        input: InputEvent,
        last_update: &Instant,
        rep: &Repeater,
        string: &mut String,
        clipboard: &mut C,
    ) -> Vec<Edit> {
        match (input, &self.held_input) {
            (InputEvent::Update, &Some(HeldInput { input, start_time })) => {
                let n = rep.count(last_update.duration_since(start_time));
                return std::iter::repeat_with(|| self.press(input, string, clipboard))
                    .take(n)
                    .filter_map(|x| x)
                    .collect();
            }

            (InputEvent::Press(input), _) => {
                self.held_input = Some(HeldInput::now(input));
                return self.press(input, string, clipboard)
                    .as_slice()
                    .to_vec();
            }

            (InputEvent::Release(input), Some(held)) if input == held.input => {
                self.held_input = None;
            }

            _ => {}
        };

        Vec::new()
    }
}
