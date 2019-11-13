// use std::collections::HashMap;
use std::marker::PhantomData;
// use std::num::Wrapping;
use std::ops::Index;
use std::sync::Arc;

use super::*;

// const nodes: HashMap<>

// struct NDTreeHash

pub type NDTree<T: CellType, D: Dim> = Arc<NDTreeNode<T, D>>;

pub struct NDTreeNode<T: CellType, D: Dim> {
    layer: usize,
    child: NDTreeChild<T, D>,
    phantom: PhantomData<D>,
}

pub enum NDTreeChild<T: CellType, D: Dim> {
    Leaf(T),
    // I hate to use a vector for this, but until rust-lang #44580 (RFC 2000) is
    // resolved, there's no way to use D::NDIM as the array size. It might be
    // worth implementing a custom unsafe type for this.
    Branch(Vec<NDTree<T, D>>),
}

pub type NDTree1D<T> = NDTree<T, Coords1D>;
pub type NDTree2D<T> = NDTree<T, Coords2D>;
pub type NDTree3D<T> = NDTree<T, Coords3D>;
pub type NDTree4D<T> = NDTree<T, Coords4D>;
pub type NDTree5D<T> = NDTree<T, Coords5D>;
pub type NDTree6D<T> = NDTree<T, Coords6D>;

impl<T: CellType, D: Dim> NDTreeNode<T, D> {
    pub fn new() -> NDTree<T, D> {
        Self {
            layer: 0,
            child: NDTreeChild::Leaf(T::default()),
            phantom: PhantomData,
        }
        .intern()
    }
    fn intern(self) -> Arc<Self> {
        // TODO implement hashing and interning
        Arc::new(self)
    }
    fn get_branch_index(&self, coords: Coords<D>) -> usize {
        // Take the Nth bit (where N = self.layer) of each coordinate, and use
        // those to form an integer index.
        let mut index: usize = 0;
        for axis in D::axes() {
            index <<= 1;
            index |= ((coords[axis] >> self.layer) & 1) as usize;
        }
        index
    }
    fn expand_leaf(layer: usize, cell_value: T) -> NDTreeChild<T, D> {
        NDTreeChild::Branch(vec![
            Arc::new(NDTreeNode {
                layer: layer - 1,
                child: NDTreeChild::Leaf(cell_value),
                phantom: PhantomData
            });
            1 << D::NDIM
        ])
    }
    pub fn get_cell(&self, coords: Coords<D>) -> T {
        match &self.child {
            NDTreeChild::Leaf(cell) => *cell,
            NDTreeChild::Branch(children) => {
                children[self.get_branch_index(coords)].get_cell(coords)
            }
        }
    }
    pub fn set_cell(&self, coords: Coords<D>, cell_value: T) -> NDTree<T, D> {
        NDTreeNode {
            child: match &self.child {
                NDTreeChild::Leaf(_) => NDTreeChild::Leaf(cell_value),
                NDTreeChild::Branch(children) => {
                    let mut ret = children.clone();
                    let branch_index = self.get_branch_index(coords);
                    ret[branch_index] = ret[branch_index].set_cell(coords, cell_value);
                    NDTreeChild::Branch(ret)
                }
            },
            ..*self
        }
        .intern()
        // let new_child = match self.child {
        //     NDTreeChild::Branch(children) => children.clone()
        //     // NDTreeChild::Leaf(cell) => NDTreeChild::Branch([; ]),
        // }
        // Self {
        //     child: new_child,
        //     ..*self
        // }
    }
}

// impl<T: CellType, D: Dim> Index<Coords<D>> for NDTree<T, D> {
//     type Output = T;
//     fn index(&self, coords: Coords<D>) -> &T {
//         match &self.child {
//             NDTreeChild::Leaf(cell) => cell,
//             NDTreeChild::Branch(children) => &children[self.get_branch_index(coords)][coords],
//         }
//     }
// }

// pub struct NDBranch<T: CellType, D: Dim> {
//     // hash: Wrapping<usize>,
//     phantom: PhantomData<D>,
//     layer: usize,
//     children: [NDTree<T, D>],
// }

// impl<T: CellType, D: Dim> NDBranch<T, D> {
//     fn new(layer: usize) -> Self {
//         Self {
//             phantom: PhantomData,
//             layer: layer,
//             children: [Arc::new(NDTree::Empty); 8],
//             // children: [Arc::new(NDTree::Empty); D::NDIM],
//         }
//     }
//     fn get_cell()
// }
