use std::iter::Peekable;

/// An iterator
pub struct Incredible<A: Iterator, B: Iterator, F> {
    a: Peekable<A>,
    b: Peekable<B>,
    f: F,
}

impl<A: Iterator, B: Iterator, F> Clone for Incredible<A, B, F>
where
    F: Clone,
    Peekable<A>: Clone,
    Peekable<B>: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        Self {
            a: self.a.clone(),
            b: self.b.clone(),
            f: self.f.clone(),
        }
    }
}

impl<A: Iterator, B: Iterator, F> Incredible<A, B, F> {
    fn new(a: A, b: B, f: F) -> Self {
        Self {
            a: a.peekable(),
            b: b.peekable(),
            f,
        }
    }

    /// Extract what remains of the contained iterators.
    #[inline]
    pub fn remainder(self) -> (std::iter::Peekable<A>, std::iter::Peekable<B>) {
        (self.a, self.b)
    }
}

pub trait IncredibleExt: IntoIterator {
    /// Named in reference to the scene from "The Incredibles" where a robot is updated when a hero defeats it, and the next
    /// hero is not battled until the current one is defeated.
    ///
    /// This is a highly specialized combination of [`std::iter::Zip`], [`std::iter::FilterMap`] and [`std::iter::FlatMap`]
    /// which increments the outer iterator only when the closure returns [`None`] (otherwise repeating it), only increments
    /// the inner iterator when the closure returns [`Some`] (otherwise repeating it), and returns the output of the closure
    /// if it is [`Some`].
    ///
    /// If either iterator returns [`None`], the iterator is finished and will only ever return [`None`] from that point on.
    /// The iterators can then be inspected by calling [`Incredible::remainder`].
    ///
    /// # Example
    /// ```
    /// for x in outer.incredible(inner, |a, b| /* ... */) {
    ///     // ...
    /// }
    /// ```
    #[inline]
    fn incredible<T, U, F>(self, inner: U, f: F) -> Incredible<Self::IntoIter, U::IntoIter, F>
    where
        Self: Sized,
        U: IntoIterator,
        F: FnMut(Self::Item, U::Item) -> Option<T>,
    {
        Incredible::new(self.into_iter(), inner.into_iter(), f)
    }
}

impl<T: IntoIterator> IncredibleExt for T {}

impl<A, B, T, F> Iterator for Incredible<A, B, F>
where
    A: Iterator<Item: Clone>,
    B: Iterator<Item: Clone>,
    F: FnMut(A::Item, B::Item) -> Option<T>,
{
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let (Some(a), Some(b)) = (self.a.peek(), self.b.peek()) {
                let item = (self.f)(a.clone(), b.clone());
                if item.is_some() {
                    _ = self.b.next();
                    break item;
                } else {
                    _ = self.a.next();
                }
            } else {
                break None;
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (a_min, a_max) = self.a.size_hint();
        let (b_min, b_max) = self.b.size_hint();
        let lower = a_min.max(b_min);
        let upper = if let (Some(a_max), Some(b_max)) = (a_max, b_max) {
            Some(a_max.max(b_max))
        } else {
            None
        };
        (lower, upper)
    }
}

impl<A: Iterator, B: Iterator, F> std::iter::FusedIterator for Incredible<A, B, F> where Self: Iterator {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test0() {

    }
}
