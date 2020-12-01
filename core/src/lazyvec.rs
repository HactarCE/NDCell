//! Lazy vector.

use std::ops::Index;

/// An "infinite" vector that lazily generates values in order, with each next
/// value based on the previous ones.
#[derive(Debug, Clone)]
pub struct LazyVec<T, F> {
    values: Vec<T>,
    generator: F,
}
impl<T, F: FnMut(&[T]) -> T> From<F> for LazyVec<T, F> {
    fn from(generator: F) -> Self {
        Self::new(generator)
    }
}
impl<T, F> LazyVec<T, F>
where
    F: FnMut(&[T]) -> T,
{
    pub fn new(generator: F) -> Self {
        Self {
            values: vec![],
            generator,
        }
    }

    pub fn get(&mut self, index: usize) -> &T {
        if self.values.len() <= index {
            let lacking = index - self.values.len() + 1;
            self.values.reserve(lacking);
            while self.values.len() <= index {
                let next_value = (self.generator)(self.values.as_slice());
                self.values.push(next_value);
            }
        }
        &self.values[index]
    }
}
impl<T, F> Index<usize> for LazyVec<T, F> {
    type Output = T;

    fn index(&self, index: usize) -> &T {
        &self.values[index]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lazy_vec() {
        let mut integers = 0..;
        let mut lazy_squares = LazyVec::from(|previous: &[i32]| {
            let n = previous.len() as i32;
            assert_eq!(n, integers.next().unwrap());
            n * n
        });
        assert_eq!(100, *lazy_squares.get(10));
        assert_eq!(25, *lazy_squares.get(5));
        assert_eq!(0, *lazy_squares.get(0));
        assert_eq!(1, *lazy_squares.get(1));
        assert_eq!(121, *lazy_squares.get(11));
        assert_eq!(4, *lazy_squares.get(2));
        for i in 0..=11 {
            assert_eq!(i * i, lazy_squares[i as usize]);
        }
    }
}
