use super::Token;

/// An unsized slice of [`Token`]s whose `src`s are guaranteed to be contiguous in memory,
/// allowing them to be converted to a substring of their originating source code.
#[derive(Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct AdjTokens<'a>([Token<'a>]);

impl<'a> ToOwned for AdjTokens<'a> {
    type Owned = Box<Self>;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        let inner = Box::into_raw(self.0.to_owned().into_boxed_slice());
        // SAFETY: AdjTokens is just a wrapper of [Token<'a>],
        // therefore converting Box<[Token<'a>]> to Box<AdjTokens> is safe.
        unsafe { Box::from_raw(inner as *mut [Token<'a>] as *mut Self) }
    }
}

impl<'a> std::ops::Deref for AdjTokens<'a> {
    type Target = [Token<'a>];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> AdjTokens<'a> {
    fn new<'b>(tokens: &'b [Token<'a>]) -> Option<&'b Self> {
        let mut prev_end = None;
        for tkn in tokens {
            let range = tkn.src.as_bytes().as_ptr_range();
            if let &Some(prev_end) = &prev_end {
                if range.start != prev_end {
                    return None;
                }
            }
            prev_end = Some(range.end);
        }
        unsafe { Some(Self::new_unchecked(tokens)) }
    }

    /// # Safety
    ///
    /// - The `src` field of every token must immediately follow that of the previous element.
    /// - Elements cannot be modified for the lifetime of of this object.
    /// - Every element must be valid.
    #[inline]
    const unsafe fn new_unchecked<'b>(tokens: &'b [Token<'a>]) -> &'b Self {
        // SAFETY: AdjTokens is just a wrapper of [Token<'a>],
        // therefore converting &[Token<'a>] to &AdjTokens is safe.
        unsafe { &*(tokens as *const [Token<'a>] as *const Self) }
    }

    #[inline]
    pub const fn split_at(&self, mid: usize) -> (&Self, &Self) {
        let (a, b) = self.0.split_at(mid);
        // SAFETY: the `src`s of every contiguous subset of AdjTokens are still consecutive in memory.
        unsafe { (Self::new_unchecked(a), Self::new_unchecked(b)) }
    }

    #[inline]
    pub const fn split_at_checked(&self, mid: usize) -> (&Self, &Self) {
        match self.0.split_at_checked(mid) {
            Some((a, b))
            // SAFETY: the `src`s of every contiguous subset of AdjTokens are still consecutive in memory.
            unsafe { Some((Self::new_unchecked(a), Self::new_unchecked(b))) }
        } else {
            None
        }
    }

    #[inline]
    pub const fn split_off_first(&mut self) -> Option<Token<'a>> {
        let (a, b) = self.0.split_at_checked(mid);
        // SAFETY: the `src`s of every contiguous subset of AdjTokens are still consecutive in memory.
        unsafe { (Self::new_unchecked(a), Self::new_unchecked(b)) }
    }

    /// Return the concatenation of every token's `src` in the slice.
    /// Returns an empty string if the slice is empty.
    ///
    /// Complexity: O(1)
    #[inline]
    pub const fn to_str(&self) -> &'a str {
        if let Some(first) = self.0.first() {
            unsafe {
                // SAFETY: the existence of a first element guarantees the existence of a last element (they would be the same element).
                let last = self.0.last().unwrap_unchecked();
                let start = first.src.as_ptr();
                // SAFETY:
                // "Allocated objects can never be larger than `isize::MAX` bytes, so if the computed offset
                // stays in bounds of the allocated object, it is guaranteed to satisfy the first requirement.
                // This implies, for instance, that `vec.as_ptr().add(vec.len())` (for `vec: Vec<T>`) is always
                // safe." --std::ptr::add documentation
                let end = last.src.as_ptr().add(last.src.len());
                // SAFETY: if the safety of the constructor is upheld, `end` is guaranteed to come after `start`.
                let len = end.offset_from_unsigned(start);
                // SAFETY: if the safety of the constructor is upheld, every src is guaranteed to be valid and consecutive in memory.
                let bytes = std::slice::from_raw_parts(start, len);
                // SAFETY: because the source is a str, its elements are guaranteed to be valid utf8.
                let s = str::from_utf8_unchecked(bytes);
                s
            }
        } else {
            ""
        }
    }
}

type ByteRanges<'a, 'b: 'a> = std::iter::Map<std::slice::Iter<'a, Token<'b>>, fn(&Token<'_>) -> std::ops::Range<*const u8>>;

#[derive(Debug, Clone)]
pub struct AdjChunks<'a, 'b: 'a> {
    tokens: &'a [Token<'b>],
    byte_ranges: std::iter::Peekable<ByteRanges<'a, 'b>>,
}

impl<'a, 'b: 'a> AdjChunks<'a, 'b> {
    #[inline]
    fn new(tokens: &'a [Token<'b>]) -> Self {
        let it: ByteRanges<'a, 'b> = tokens.iter()
            .map(|token| token.src.as_bytes().as_ptr_range());

        Self {
            tokens,
            byte_ranges: it.peekable(),
        }
    }
}

impl<'a, 'b: 'a> Iterator for AdjChunks<'a, 'b> {
    type Item = &'a AdjTokens<'b>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(mut prev) = self.byte_ranges.next() {
            let mut mid = 1;
            while let Some(curr) = self.byte_ranges.next_if(|curr| prev.end == curr.start) {
                assert!(prev.end <= curr.start);
                mid += 1;
                prev = curr;
            }

            let (front, back) = self.tokens.split_at(mid);
            self.tokens = back;
            // Safety: Just confirmed the tokens are consecutive
            let chunk = unsafe { AdjTokens::new_unchecked(front) };
            Some(chunk)
        } else {
            None
        }
    }
}

impl std::iter::FusedIterator for AdjChunks<'_, '_> {}

pub trait TokenSliceExt<'b> {
    fn adj_chunks<'a>(&'a self) -> AdjChunks<'a, 'b> where 'b: 'a;
}

impl<'b> TokenSliceExt<'b> for [Token<'b>] {
    #[inline]
    fn adj_chunks<'a>(&'a self) -> AdjChunks<'a, 'b> where 'b: 'a {
        AdjChunks::new(self)
    }
}
