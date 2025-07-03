#![allow(dead_code, reason = "WIP")]

pub struct Snippet<'a> {
    pub text: std::borrow::Cow<'a, str>,
    pub insertion_ranges: Vec<std::ops::Range<usize>>,
}

impl<'a> Snippet<'a> {
    pub const fn literal(text: &'a str) -> Self {
        Self {
            text: std::borrow::Cow::Borrowed(text),
            insertion_ranges: Vec::new(),
        }
    }

    fn make_insertion_ranges(s: &str) -> Vec<std::ops::Range<usize>> {
        s
            .match_indices("$(")
            .filter_map(|(start, _)| {
                let substr = &s[start + 2..];
                substr
                    .find(')')
                    .filter(|&len| substr[..len].parse::<usize>().is_ok())
                    .map(|len| start..start + len + 1)
            })
            .collect()
    }

    pub fn new(text: &'a str) -> Self {
        Self {
            insertion_ranges: Self::make_insertion_ranges(text),
            text: std::borrow::Cow::Borrowed(text),
        }
    }

    pub fn new_owned(text: String) -> Self {
        Self {
            insertion_ranges: Self::make_insertion_ranges(&text),
            text: std::borrow::Cow::Owned(text),
        }
    }

    fn apply<'b>(&self, insertions: impl ExactSizeIterator<Item = &'b str> + DoubleEndedIterator) -> String {
        let mut s = self.text.to_string();
        for (range, rep) in self.insertion_ranges[0..insertions.len()].iter().cloned().zip(insertions).rev() {
            s.replace_range(range, rep);
        }
        s
    }
}