# NDCell

_Any number of dimensions_  
_Any number of states_  
_Any neighborhood range_  
_Any computable transition function_

**Simulate _any_ cellular automaton**

An N-dimensional cellular automaton simulation program. Maybe. Someday.

## Short-term to-do list

- [x] Render 2D grids
    + [x] ... at any power-of-2 zoom level
    + [x] ... at 60 FPS
        * [x] Render grid separately from cells
        * [x] Skip empty nodes
        * [x] Render in 256x256 chunks
        * [x] Skip empty chunks
        * [x] Cache chunks between frames
- [ ] Control simulation from GUI
    + [ ] Undo/reset
    + [ ] Change sim step
    + [ ] Jump to generation
- [ ] Read simple 2D RLE files from Golly
- [ ] Begin work on 3D rendering

## Implementation status

### Simulation

#### Rules

##### 1D

- [ ] Rule 110

##### 2D

- [x] Conway's Game of Life
- [ ] Langton's Ant
- [ ] Wireworld

##### 3D

- [ ] Langton's Ant (3D generalization)
- [ ] Wireworld

##### Generalized

- [ ] Totalistic
- [ ] [Turmite](https://en.wikipedia.org/wiki/Turmite)

#### Grid geometry/topology

- [x] Unbounded (infinite)
    + [x] Up to ~ ±2^63 (or ±2^31 on 32-bit platforms)
    + [ ] Beyond ±2^63 using `BigInt`s [(probably not happening for a while)](#big-integers)
- [ ] Bounded (finite)
- [ ] Partially bounded (e.g. tube)
- [ ] Edge conditions
    + Loop (e.g. torus)
    + Loop with offset (e.g. twisted torus)
    + Flip (e.g. Möbius loop)

### UI

- [ ] "Root" window that can toggle other windows

### Grid display

- [ ] A 1D grid
    + [ ] as "barcode"
    + [ ] as squares
    + [ ] as 2D spacetime
- [ ] A 2D grid
    + [x] as 2D space
    + [ ] as 3D spacetime
    + [ ] as 2D slice of spacetime
- [ ] A 3D grid
    + [ ] as 2D slice
    + [ ] as 3D space
- [ ] N-dimensional grid
    + [ ] as 2D slice
    + [ ] as 3D slice
    + [ ] as 2D slice of spacetime
    + [ ] as 3D slice of spacetime

### Cell display

- [x] Cell borders (2D)
- [ ] Cell gaps (3D)
- [x] Color cell depending on state
- [ ] Custom cell sprite (2D)
- [ ] Custom cell shape (2D)
- [ ] Custom cell model (3D)

### Editor

- [ ] Movement
    + [ ] in 1D
    + [ ] in 2D
    + [ ] in 3D
- [ ] Toggle/cycle cell state
    + [ ] in 1D
    + [ ] in 2D
    + [ ] in 3D
- [ ] More advanced editing (?)

## Big Integers

tl;dr, I want [generic associated types](https://github.com/rust-lang/rfcs/blob/master/text/1598-generic_associated_types.md) first, so I'm waiting on [rust-lang#44265](https://github.com/rust-lang/rust/issues/44265). I will probably even switch to the experimental compiler for this, once it's usable.

Golly is able to simulate practically infinite grids that are upwards of 10^100 cells wide. Storing such grids is not challenging; an `NdTree` can store an `n^d` grid in `log(n)*2^d` space (where `d` is the number of dimensions). But indexing such a massive grid (usually for I/O purposes, like displaying it) requires a numeric type that can handle precise integer values well beyond what fits in a standard 64-bit or even 128-bit integer. Thankfully, such a data type exists: the [`BigInt`](https://rust-num.github.io/num/num/struct.BigInt.html).

So why haven't I refactored NDCell to use BigInts everywhere? Well BigInts are a little heavier, a little slower to allocate (they live on the heap, not the stack), and a little harder to copy (they implement `Clone` but not `Copy`) than primitive integers, so I'd rather only use them where necessary. This wouldn't be a big deal in a simple 2D or 3D simulation; just make a few extra traits and structs for the new types. But because NDCell supports an arbitrary number of dimensions, I've made a type alias for each dimensionality (`Dim1D`, `Dim2D`, ..., `Dim6D`). That type alias corresponds to an array of coordinates; e.g. `Dim3D` is an alias for `[isize; 3]` (using an `isize` for each coordinate).

```rust
trait Dim {
    const NDIM: usize,
    ...
}

pub type Dim1D = [isize; 1];
pub type Dim2D = [isize; 2];
...
pub type Dim6D = [isize; 6];

impl Dim for Dim1D {
    const NDIM: usize = 1;
    ...
}
...
```

Ideally, I would be able to use whatever index type suits the situation; so I could have one vector type `NdVec<T: Coordinate, D: Dim>` (as opposed to the current `NdVec<D: Dim>` that always stores `isize`s), where `T` could be `isize` or `BigInt` depending on the required precision. This could even result in a net _reduction_ in lines of code, since branch indices could be replaced with `NdVec<bool, D>`.

```rust
trait Dim {
    const NDIM: usize,
    type Array<T>: Index<usize, Output = T> + IndexMut<usize>;
}

struct Dim1D;
struct Dim2D;
...
struct Dim6D;

impl Dim for Dim1D {
    const NDIM: usize = 1;
    type Array<T> = [T; 1];
}
```

Isn't that much nicer? It's much more general, at least Now we can also convert between them; for example, the following trait `impl` would allow converting between `NdVec<BigInt>` and `NdVec<isize>` using `std::convert::From` and `std::convert::TryInto`:

```rust
impl<T1, T2> From<NdVec<T1>> for NdVec<T2> where T1: From<T2> { ... }
```

## Possible future optimizations

- 2D rendering
    + [ ] Multithread quadtree reading
    + [ ] LoD rendering (render chunks at lower resolution initially, then improve resolution on the next frame)
