use raylib::prelude::*;
use super::parse_color::RichColor;

pub trait EnrichEx {
    fn enrich(&self, root_color: Color, initial: &str) -> Enrich<'_>;
}

impl EnrichEx for str {
    fn enrich(&self, root_color: Color, initial: &str) -> Enrich<'_> {
        Enrich::new(self, root_color, initial)
    }
}

enum StackMutation<T> {
    Push(T),
    Pop,
}

fn parse_color_node(s: &str) -> Option<RichColor> {
    s
        .strip_suffix('>')
            .expect("should be guarded by filter_map")
        .strip_prefix("<color")
        .and_then(|s| s
            .trim_start_matches(' ')
            .strip_prefix('=')
        )
        .and_then(|s| s
            .trim_matches(' ')
            .parse()
            .ok()
        )
}

fn color_region(s: &str) -> (&str, &str, Option<StackMutation<Color>>) {
    s
        .match_indices(['<'])
        .map(|(i, _)| s.split_at(i))
        .filter_map(|(pre, post)| post
            .find('>')
            .map(|pos| (pre, post.split_at(pos + 1)))
        )
        .find_map(|(pre, (ext, post))| {
            (ext == "</color>")
                .then_some(StackMutation::Pop)
                .or_else(||
                    parse_color_node(ext)
                        .map(|RichColor(color)| StackMutation::Push(color))
                )
                .map(|sm| (pre, post, Some(sm)))
        })
        .unwrap_or((s, "", None))
}

#[derive(Clone)]
pub struct Enrich<'a> {
    text: &'a str,
    root_color: Color,
    color_stack: Vec<Color>,
}

impl<'a> Enrich<'a> {
    pub fn new(text: &'a str, root_color: Color, initial: &str) -> Self {
        let mut color_stack = Vec::new();
        let mut s = initial;
        let mut c;
        while !s.is_empty() {
            (_, s, c) = color_region(s);
            if let Some(c) = c {
                match c {
                    StackMutation::Push(value) => color_stack.push(value),
                    StackMutation::Pop => {
                        if cfg!(debug_assertions) && color_stack.is_empty() {
                            println!("built-in messages should not contain excessive color stack pops");
                        }
                        color_stack.pop();
                    }
                }
            }
        }

        Self {
            text,
            root_color,
            color_stack,
        }
    }
}

impl<'a> Iterator for Enrich<'a> {
    type Item = (&'a str, Color);

    fn next(&mut self) -> Option<Self::Item> {
        while !self.text.is_empty() {
            let (result, sm);
            (result, self.text, sm) = color_region(self.text);
            let segment_color = self.color_stack.last().copied().unwrap_or(self.root_color);
            if let Some(sm) = sm {
                match sm {
                    StackMutation::Push(value) => self.color_stack.push(value),
                    StackMutation::Pop => {
                        if cfg!(debug_assertions) && self.color_stack.is_empty() {
                            println!("built-in messages should not contain excessive color stack pops");
                        }
                        self.color_stack.pop();
                    }
                }
            }

            if !result.is_empty() {
                return Some((result, segment_color));
            }
        }
        None
    }
}
