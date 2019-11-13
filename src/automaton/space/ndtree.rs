// use std::collections::HashMap;
use std::marker::PhantomData;
// use std::num::Wrapping;
use std::ops::Index;
use std::sync::Arc;

use super::*;

// const nodes: HashMap<>

// struct NDTreeHash

pub struct NDTree<T: CellType, D: Dim> {
    layer: usize,
    node: NDNode<T, D>,
    phantom: PhantomData<D>,
}

pub enum NDNode<T: CellType, D: Dim> {
    Leaf(T),
    Branch(Arc<[NDTree<T, D>]>),
}

pub type NDTree1D<T> = NDTree<T, Coords1D>;
pub type NDTree2D<T> = NDTree<T, Coords2D>;
pub type NDTree3D<T> = NDTree<T, Coords3D>;
pub type NDTree4D<T> = NDTree<T, Coords4D>;
pub type NDTree5D<T> = NDTree<T, Coords5D>;
pub type NDTree6D<T> = NDTree<T, Coords6D>;

impl<T: CellType, D: Dim> NDTree<T, D> {
    pub fn new() -> Self {
        Self {
            layer: 0,
            node: NDNode::Leaf(T::default()),
            phantom: PhantomData,
        }
    }
}

impl<T: CellType, D: Dim> Index<Coords<D>> for NDTree<T, D> {
    type Output = T;
    fn index(&self, coords: Coords<D>) -> &T {
        match &self.node {
            NDNode::Leaf(cell) => cell,
            NDNode::Branch(children) => {
                // Take the Nth bit (where N = self.layer) of each coordinate, and use
                // those to form an integer index.
                let mut index: usize = 0;
                for axis in D::axes() {
                    index <<= 1;
                    index |= ((coords[axis] >> self.layer) & 1) as usize;
                }
                &children[index][coords]
            }
        }
    }
}

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
