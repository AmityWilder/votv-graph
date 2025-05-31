#![feature(char_max_len, variant_count, iterator_try_reduce)]

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

impl Direction {
    #[inline]
    pub const fn is_vertical(self) -> bool {
        (self as u8 & 2) != 0
    }

    #[inline]
    pub const fn is_horizontal(self) -> bool {
        !self.is_vertical()
    }

    #[inline]
    pub const fn is_backward(self) -> bool {
        (self as u8 & 1) != 0
    }

    #[inline]
    pub const fn is_forward(self) -> bool {
        !self.is_backward()
    }
}

const _: () = {
    assert!(Direction::Right.is_forward());
    assert!(Direction::Right.is_horizontal());
    assert!(Direction::Left.is_backward());
    assert!(Direction::Left.is_horizontal());
    assert!(Direction::Down.is_forward());
    assert!(Direction::Down.is_vertical());
    assert!(Direction::Up.is_backward());
    assert!(Direction::Up.is_vertical());
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum CursorStep {
    /// column/row
    #[default]
    Single,
    /// word/page
    Segment,
    /// line/document
    Full,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MoveCursor(u8);

impl MoveCursor {
    const STEP_BY_OFFSET: u32 = u32::MIN;

    const STEP_BY_BITS: u8 =
        CursorStep::Single as u8 |
        CursorStep::Segment as u8 |
        CursorStep::Full as u8;

    const STEP_BY_MASK: u8 = Self::STEP_BY_BITS << Self::STEP_BY_OFFSET;

    const DIRECTION_OFFSET: u32 =
        Self::STEP_BY_MASK
            .next_power_of_two()
            .trailing_zeros();

    const DIRECTION_BITS: u8 =
        Direction::Right as u8 |
        Direction::Left as u8 |
        Direction::Down as u8 |
        Direction::Up as u8;

    const DIRECTION_MASK: u8 = Self::DIRECTION_BITS << Self::DIRECTION_OFFSET;

    const IS_SELECTING_OFFSET: u32 =
        Self::DIRECTION_MASK
            .next_power_of_two()
            .trailing_zeros();

    const IS_SELECTING_BITS: u8 =
        false as u8 |
        true as u8;

    const IS_SELECTING_MASK: u8 = Self::IS_SELECTING_BITS << Self::IS_SELECTING_OFFSET;

    #[inline]
    pub const fn new(step_by: CursorStep, direction: Direction, is_selecting: bool) -> Self {
        Self(
            ((is_selecting as u8) << Self::IS_SELECTING_OFFSET) |
            ((direction as u8) << Self::DIRECTION_OFFSET) |
            ((step_by as u8) << Self::STEP_BY_OFFSET)
        )
    }

    #[inline]
    pub const fn step_by(self) -> CursorStep {
        unsafe { std::mem::transmute(self.0 & Self::STEP_BY_MASK) }
    }

    #[inline]
    pub const fn direction(self) -> Direction {
        unsafe { std::mem::transmute((self.0 & Self::DIRECTION_MASK) >> 2) }
    }

    #[inline]
    pub const fn is_selecting(self) -> bool {
        (self.0 & Self::IS_SELECTING_MASK) != 0
    }

    #[inline]
    pub fn apply(&self, string: &str, page_lines: usize, pos: usize) -> usize {
        assert!(pos <= string.len());

        let dir = self.direction();
        let step = self.step_by();

        let size = {
            let within = if dir.is_forward() { &string[pos..] } else { &string[..pos] };

            match (step, dir) {
                (CursorStep::Full, Direction::Down | Direction::Up)  => None,

                (CursorStep::Single, Direction::Right | Direction::Left) => {
                    let mut it = within.chars().map(|ch| ch.len_utf8());
                    let next = if dir.is_forward() { Iterator::next } else { DoubleEndedIterator::next_back };
                    next(&mut it)

                }

                (CursorStep::Segment, Direction::Right | Direction::Left) => {
                    let mut it = within.words().map(|s| s.len());
                    let next = if dir.is_forward() { Iterator::next } else { DoubleEndedIterator::next_back };
                    next(&mut it)

                }

                (CursorStep::Full, Direction::Right | Direction::Left) => {
                    let mut it = within.lines().map(|s| s.len());
                    let next = if dir.is_forward() { Iterator::next } else { DoubleEndedIterator::next_back };
                    next(&mut it)

                }

                (CursorStep::Single | CursorStep::Segment, Direction::Down | Direction::Up) => {
                    // in chars, not bytes
                    let col = string[..pos].chars().rev()
                        .position(|ch| ch == '\n')
                        .unwrap_or(pos);

                    let mut it = within.lines().map(|s| s.len());
                    let next = if dir.is_forward() { Iterator::next } else { DoubleEndedIterator::next_back };

                    let n = match step {
                        CursorStep::Single => 1,
                        CursorStep::Segment => page_lines,
                        CursorStep::Full => unreachable!(),
                    };

                    (0..n)
                        .map(|_| next(&mut it))
                        .try_fold(col, |acc, item| {
                            item.map(|x| acc + x)
                        })
                        .and_then(|len| within[len..]
                            .char_indices()
                            .nth(col)
                            .map(|(i, _)| i)
                        )
                }
            }.unwrap_or_else(|| within.len())
        };

        if dir.is_forward() { pos + size } else { pos - size }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Input {
    /// A single unicode character (shift/caps should be externally accounted)
    Symbol(char),
    /// Modify the cursor position relative to where it is currently
    MoveCursor(MoveCursor),
    /// Set the selection to the entire document
    SelectAll,
    /// Copy the selection to the clipboard and erase the selected text
    Cut,
    /// Copy the selection to the clipboard
    Copy,
    /// Paste the clipboard over the selection
    Paste,
    /// Set the selection to only the head
    Deselect,
    /// Erase a step to the left
    DeleteBefore(CursorStep),
    /// Erase a step to the right
    DeleteAfter(CursorStep),
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
        page_lines: usize,
        clipboard: &mut C,
    ) -> Option<Edit> {
        let selection = self.selection_range();

        if matches!(&input, Input::Cut | Input::Copy) {
            clipboard.set(Some(&string[selection.clone()]));
        }

        match &input {
            Input::MoveCursor(movement) => {
                self.selection_tail = movement.apply(string, page_lines, self.selection_tail);

                if !movement.is_selecting() {
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
            | Input::DeleteBefore(_)
            | Input::DeleteAfter(_)
                => {
                    let mut buf;
                    let insertion = match input {
                        Input::Symbol(ch) => {
                            buf = [0; char::MAX_LEN_UTF8];
                            ch.encode_utf8(&mut buf)
                        }

                        Input::Paste => clipboard.get().unwrap_or(""),

                        | Input::Cut
                        | Input::DeleteBefore(_)
                        | Input::DeleteAfter(_)
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
        page_lines: usize,
        string: &mut String,
        clipboard: &mut C,
    ) -> Vec<Edit> {
        match (input, &self.held_input) {
            (InputEvent::Update, &Some(HeldInput { input, start_time })) => {
                let n = rep.count(last_update.duration_since(start_time));
                return std::iter::repeat_with(|| self.press(input, string, page_lines, clipboard))
                    .take(n)
                    .filter_map(|x| x)
                    .collect();
            }

            (InputEvent::Press(input), _) => {
                self.held_input = Some(HeldInput::now(input));
                return self.press(input, string, page_lines, clipboard)
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

#[cfg(test)]
mod test {
    use super::*;

    const ASCII_TEST_STR: &str = "apple orange\nbanana mango\nstrawberry kiwi\npineapple";
    const UTF8_TEST_STR: &str = "appl𒉌e oran௵ge\nbana𒎔na manʩgo\nstr𒈝awஹberry𒈼 kiwi\npine𒈍app௹le";

    #[test]
    fn test_len_ascii_single_right() {
        let s = ASCII_TEST_STR;
        for (head, expect) in [
            (0, 1),             // start
            (6, 7),             // arbitrary (within first line)
            (12, 13),           // line boundary
            (6, 7),             // arbitrary (within arbitrary line)
            (6, 7),             // arbitrary (within last line)
            (s.len(), s.len()), // document boundary
        ] {
            let tail = MoveCursor::new(CursorStep::Single, Direction::Right, true).apply(s, 1, head);
            assert_eq!(tail, expect);
        }
    }

    #[test]
    fn test_len_ascii_single_left() {
        let s = ASCII_TEST_STR;
        for (head, expect) in [
            (s.len(), s.len() - 1), // end
            (7, 6),                 // arbitrary (within first line)
            (13, 12),               // line boundary
            (7, 6),                 // arbitrary (within arbitrary line)
            (7, 6),                 // arbitrary (within last line)
            (0, 0),                 // document boundary
        ] {
            let tail = MoveCursor::new(CursorStep::Single, Direction::Left, true).apply(s, 1, head);
            assert_eq!(tail, expect);
        }
    }
}
