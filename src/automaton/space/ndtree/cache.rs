use seahash::SeaHasher;
use std::cell::RefCell;
use std::hash::{BuildHasherDefault, Hasher};
use std::rc::{Rc, Weak};
use weak_table::WeakHashSet;

use super::*;

pub type NdTreeCache<T, D> =
    Rc<RefCell<WeakHashSet<Weak<NdTreeNode<T, D>>, BuildHasherDefault<SeaHasher>>>>;

impl<T: CellType, D: Dim> Eq for NdTreeNode<T, D> {}
impl<T: CellType, D: Dim> PartialEq for NdTreeNode<T, D> {
    fn eq(&self, rhs: &Self) -> bool {
        // Check for pointer equality (very fast; guarantees true).
        std::ptr::eq(self, rhs)
            // If that fails, check hash codes (very fast; guarantees false).
            || (self.hash_code == rhs.hash_code
                // If neither of those worked, we have to check the hard way.
                && self.layer == rhs.layer
                && self.child == rhs.child)
    }
}

impl<T: CellType, D: Dim> Hash for NdTreeNode<T, D> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        // We already cached our own hash; just rehash that if you want to.
        self.hash_code.hash(hasher);
    }
}
