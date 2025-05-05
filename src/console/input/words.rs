pub trait WordsEx {
    fn words(&self) -> Words<'_>;
}

impl WordsEx for str {
    fn words(&self) -> Words<'_> {
        Words { src: self }
    }
}

const BRACKET_PAIRS: [(char, char); 4] = [
    ('[', ']'),
    ('(', ')'),
    ('{', '}'),
    ('<', '>'),
];

fn is_word_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

pub struct Words<'a> {
    src: &'a str,
}

impl<'a> Iterator for Words<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.src.is_empty() { return None; }
        let st = self.src.trim_start();
        let mid = if let Some(next_char) = st.chars().next() {
            let trimmed_len = if is_word_char(next_char) {
                st.trim_start_matches(is_word_char).len()
            } else {
                let trimmed = st.trim_start_matches(next_char);
                BRACKET_PAIRS.iter()
                    .copied()
                    .find(|&(a, _)| a == next_char)
                    .and_then(|(_, b)| trimmed.strip_prefix(b))
                    .unwrap_or(trimmed)
                    .len()
            };
            self.src.len() - trimmed_len
        } else {
            self.src.len()
        };
        let word;
        (word, self.src) = self.src.split_at(mid);
        (!word.is_empty()).then_some(word)
    }
}

impl<'a> DoubleEndedIterator for Words<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.src.is_empty() { return None; }
        let st = self.src.trim_end();
        let mid = if let Some(last_char) = st.chars().next_back() {
            let trimmed_len = if is_word_char(last_char) {
                st.trim_end_matches(is_word_char).len()
            } else {
                let trimmed = st.trim_end_matches(last_char);
                BRACKET_PAIRS.iter()
                    .copied()
                    .find(|&(_, b)| b == last_char)
                    .and_then(|(a, _)| trimmed.strip_suffix(a))
                    .unwrap_or(trimmed)
                    .len()
            };
            trimmed_len
        } else {
            self.src.len()
        };
        let word;
        (self.src, word) = self.src.split_at(mid);
        (!word.is_empty()).then_some(word)
    }
}
