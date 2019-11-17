use seahash::SeaHasher;
use std::hash::BuildHasherDefault;
use std::rc::Weak;
use weak_table::WeakKeyHashMap;

use super::*;

pub type NdTreeCache<T, D> =
    WeakKeyHashMap<Weak<NdTreeNode<T, D>>, CachedNodeInfo<T, D>, BuildHasherDefault<SeaHasher>>;

#[derive(Debug, Clone)]
pub struct CachedNodeInfo<T: CellType, D: Dim> {
    population: Option<usize>,
    futures: Vec<NdSubTree<T, D>>,
}

impl<T: CellType, D: Dim> Default for CachedNodeInfo<T, D> {
    fn default() -> Self {
        Self {
            population: None,
            futures: vec![],
        }
    }
}
