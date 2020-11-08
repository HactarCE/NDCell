use crate::axis::Axis;
use crate::dim::Dim;
use crate::ndrect::{self, BigRect};
use crate::ndvec::BigVec;
use crate::num::BigInt;

/// Iterator over a rectangle that iterates in normal order for the X axis but
/// reverse order for the Y, Z, W, U, and V axes. It also returns sentinels
/// whenever a row or layer ends.
#[derive(Debug, Clone)]
pub struct SemiReverseRectIter<D: Dim> {
    /// Iterator over the rectangle with non-X axes negated.
    inverted_rect_iter: ndrect::Iter<D, BigInt>,
    /// Position returned most recently.
    prev_pos: Option<BigVec<D>>,
    /// Position to yield next; if this is `None`, another element is taken from
    /// `inverted_rect_iter`.
    next_pos: Option<BigVec<D>>,
}
impl<D: Dim> SemiReverseRectIter<D> {
    pub fn new(mut r: BigRect<D>) -> Self {
        // Negate all axes except the X axis.
        for &ax in &D::axes()[1..] {
            r.negate_axis(ax);
        }

        Self {
            inverted_rect_iter: r.iter(),
            prev_pos: None,
            next_pos: None,
        }
    }
}
impl<D: Dim> Iterator for SemiReverseRectIter<D> {
    type Item = SemiReverseRectIterItem<D>;

    fn next(&mut self) -> Option<Self::Item> {
        // If we returned a sentinel for the last item, return the actual
        // position now.
        if let Some(pos) = self.next_pos.take() {
            return Some(SemiReverseRectIterItem::Pos(pos));
        }

        let mut new_pos = self.inverted_rect_iter.next()?;
        // Undo negation of all axes except the X axis.
        for &ax in &D::axes()[1..] {
            new_pos[ax] *= -1;
        }

        // Determine the most significant axis that changed and update
        // `self.prev_pos`.
        let prev_pos = self.prev_pos.replace(new_pos.clone());
        let axis = prev_pos.and_then(|prev_pos| {
            D::axes()
                .iter()
                .copied()
                .filter(|&ax| new_pos[ax] != prev_pos[ax])
                .next_back()
        });

        match axis {
            // No need to give a sentinel for the first value or for moving
            // along the X axis.
            None | Some(Axis::X) => Some(SemiReverseRectIterItem::Pos(new_pos)),
            // If we are moving along any other axis, return a sentinel before
            // returning the new position.
            Some(other_axis) => {
                self.next_pos = Some(new_pos);
                Some(SemiReverseRectIterItem::Next(other_axis))
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SemiReverseRectIterItem<D: Dim> {
    Pos(BigVec<D>),
    Next(Axis),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::axis::{X, Y, Z};
    use crate::ndrect::IRect3D;
    use crate::ndvec::NdVec;

    #[test]
    fn test_semi_reverse_rect_iter() {
        let rect = IRect3D::span(NdVec([-10, 0, 10]), NdVec([-8, 3, 11])).to_bigrect();

        let mut it = SemiReverseRectIter::new(rect.clone());

        use SemiReverseRectIterItem::{Next, Pos};

        for z in rect.axis_range(Z).rev() {
            for y in rect.axis_range(Y).rev() {
                for x in rect.axis_range(X) {
                    let pos = NdVec([x, y.clone(), z.clone()]);
                    assert_eq!(Some(Pos(pos)), it.next());
                }
                if y != rect.min()[Y] {
                    assert_eq!(Some(Next(Y)), it.next());
                }
            }
            if z != rect.min()[Z] {
                assert_eq!(Some(Next(Z)), it.next());
            }
        }
        assert_eq!(None, it.next());
    }
}
