use std::{collections::VecDeque, ops::Range, time::{Duration, Instant}};
use raylib::prelude::*;
use crate::console_log;

pub mod words;
use words::WordsEx;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum KeyOrChar {
    Char(char),
    Key(KeyboardKey),
}

impl KeyOrChar {
    pub fn to_key(self) -> Option<KeyboardKey> {
        match self {
            KeyOrChar::Char(c) => key_from_i32(c as i32),
            KeyOrChar::Key(k) => Some(k),
        }
    }
}

pub struct ConsoleIn {
    last_keypress: Option<(KeyOrChar, Instant)>,
    history: VecDeque<String>,
    history_offset: usize,
    is_focused: bool,
    is_dirty: bool,
    current: String,
    /// Where the selection started, measured in `char`s
    selection_head: usize,
    /// Where the selection currently is, measured in `char`s
    selection_tail: usize,
}

impl ConsoleIn {
    pub const fn new() -> Self {
        Self {
            last_keypress: None,
            history: VecDeque::new(),
            history_offset: 0,
            is_focused: true,
            is_dirty: true,
            current: String::new(),
            selection_head: 0,
            selection_tail: 0,
        }
    }

    #[inline]
    pub const fn is_dirty(&self) -> bool {
        self.is_dirty
    }

    #[inline]
    pub const fn mark_clean(&mut self) {
        self.is_dirty = false;
    }

    #[inline]
    const fn minmax_selection_mut(&mut self) -> (&mut usize, &mut usize) {
        if self.selection_head <= self.selection_tail {
            (&mut self.selection_head, &mut self.selection_tail)
        } else {
            (&mut self.selection_tail, &mut self.selection_head)
        }
    }

    #[inline]
    pub const fn selection_range(&self) -> Range<usize> {
        range_from_pair(self.selection_head, self.selection_tail)
    }

    #[inline]
    pub const fn selection_tail(&self) -> usize {
        self.selection_tail
    }

    #[inline]
    pub const fn is_focused(&self) -> bool {
        self.is_focused
    }

    #[inline]
    pub const fn focus(&mut self) {
        self.is_focused = true;
        self.last_keypress = None;
    }

    #[inline]
    pub const fn unfocus(&mut self) {
        self.is_focused = false;
    }

    #[inline]
    pub const fn current(&self) -> &str {
        self.current.as_str()
    }

    fn insert_over_selection_internal(current: &mut String, selection_head: &mut usize, selection_tail: &mut usize, is_dirty: &mut bool, string: &str) {
        let selection = range_from_pair(*selection_head, *selection_tail);
        assert!(selection.start <= current.len() && selection.end <= current.len());
        if selection.is_empty() && string.is_empty() { return; }
        let new_cursor = selection.start + string.chars().count();
        current.replace_range(selection, string);
        *selection_head = new_cursor;
        *selection_tail = new_cursor;
        *is_dirty = true;
    }

    pub fn insert_over_selection(&mut self, string: &str) {
        Self::insert_over_selection_internal(
            &mut self.current,
            &mut self.selection_head,
            &mut self.selection_tail,
            &mut self.is_dirty,
            string,
        );
    }

    pub fn insert_char_over_selection(&mut self, ch: char) {
        let mut buf = [b'\0'; 4];
        self.insert_over_selection(ch.encode_utf8(&mut buf));
    }

    fn advance_len(&self, by_word: bool, direction: i8) -> usize {
        by_word.then_some(())
            .and_then(|()| {
                let mid = self.selection_tail;
                if direction > 0 {
                    self.current[mid..].words().next()
                } else {
                    self.current[..mid].words().next_back()
                }
            })
            .map_or(1, |s| s.chars().count())
    }

    fn apply_input(&mut self, rl: &mut RaylibHandle, input: KeyOrChar, is_ctrl_down: bool, is_shift_down: bool, _is_alt_down: bool) -> bool {
        match input {
            KeyOrChar::Char(ch) => {
                if is_ctrl_down && ch == 'v' {
                    if let Ok(clipboard) = rl.get_clipboard_text() {
                        self.insert_over_selection(&clipboard);
                        return true;
                    }
                } else {
                    self.insert_char_over_selection(ch);
                    return true;
                }
            }

            KeyOrChar::Key(key) => {

                let erasure = (key == KeyboardKey::KEY_DELETE) as i8 - (key == KeyboardKey::KEY_BACKSPACE) as i8;
                if erasure != 0 {
                    if self.selection_range().len() == 0 {
                        let size = self.advance_len(is_ctrl_down, erasure);
                        let total_len = self.current.len();
                        let (min, max) = self.minmax_selection_mut();
                        if erasure > 0 {
                            *max = max.saturating_add(size).min(total_len);
                        } else {
                            *min = min.saturating_sub(size);
                        }
                    }
                    self.insert_over_selection("");
                }

                let y_movement = (key == KeyboardKey::KEY_UP) as i8 - (key == KeyboardKey::KEY_DOWN) as i8;
                if y_movement != 0 {
                    self.history_offset = if y_movement > 0 {
                        if self.history_offset + 1 < self.history.len() + 1 { self.history_offset + 1 } else { 0 }
                    } else {
                        self.history_offset.checked_sub(1).unwrap_or(self.history.len() + 1 - 1)
                    };

                    Self::insert_over_selection_internal(
                        &mut self.current,
                        &mut self.selection_head,
                        &mut self.selection_tail,
                        &mut self.is_dirty,
                        self.history.get(self.history_offset)
                            .map(String::as_str)
                            .unwrap_or_default(),
                    );

                    return true;
                }

                let x_movement = (key == KeyboardKey::KEY_RIGHT) as i8 - (key == KeyboardKey::KEY_LEFT) as i8;
                if x_movement != 0 {
                    let size = self.advance_len(is_ctrl_down, x_movement);

                    self.selection_tail = if x_movement > 0 {
                        let total_len = self.current.len();
                        self.selection_tail.saturating_add(size).min(total_len)
                    } else {
                        self.selection_tail.saturating_sub(size)
                    };

                    if !is_shift_down {
                        self.selection_head = self.selection_tail;
                    }

                    return true;
                }
            }
        }
        false
    }

    pub fn update_input(&mut self, rl: &mut RaylibHandle) -> bool {
        if let Some((rep_input, _)) = &self.last_keypress {
            if rep_input.to_key().is_none_or(|k| rl.is_key_released(k)) {
                self.last_keypress = None;
            }
        }

        if self.is_focused {
            let is_ctrl_down = rl.is_key_down(KeyboardKey::KEY_LEFT_CONTROL) || rl.is_key_down(KeyboardKey::KEY_RIGHT_CONTROL);
            let is_shift_down = rl.is_key_down(KeyboardKey::KEY_LEFT_SHIFT) || rl.is_key_down(KeyboardKey::KEY_RIGHT_SHIFT);
            let is_alt_down = rl.is_key_down(KeyboardKey::KEY_LEFT_ALT) || rl.is_key_down(KeyboardKey::KEY_RIGHT_ALT);

            let input = rl.get_char_pressed().map_or_else(
                || rl.get_key_pressed().map(|k| KeyOrChar::Key(k)),
                |c| Some(KeyOrChar::Char(c)),
            );

            if let Some(input) = input {
                self.last_keypress = Some((input, Instant::now()));
                return self.apply_input(rl, input, is_ctrl_down, is_shift_down, is_alt_down);
            } else if let Some((rep_input, ref mut pressed_time)) = self.last_keypress {
                const DELAY: Duration = Duration::from_millis(550);
                const REP: Duration = Duration::from_millis(33);
                if pressed_time.elapsed() >= DELAY {
                    *pressed_time = Instant::now() - DELAY + REP;
                    return self.apply_input(rl, rep_input, is_ctrl_down, is_shift_down, is_alt_down);
                }
            }
        }
        false
    }

    pub fn submit_cmd(&mut self) -> Option<&str> {
        (!self.current.is_empty()).then(|| {
            let msg = std::mem::take(&mut self.current);
            self.history.push_front(msg.clone());
            self.history_offset = self.history.len();
            console_log!(Ghost, "{msg}");
            self.history.front()
                .map(String::as_str)
                .expect("should have at least one element after push")
        })
    }
}

#[inline]
pub const fn range_from_pair(a: usize, b: usize) -> Range<usize> {
    if a <= b { a..b } else { b..a }
}
