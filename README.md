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
    + [x] ... at _any_ zoom level
    + [x] ... at 60 FPS
    + [x] ... by traversing the quadtree on the GPU
- [x] Control simulation from GUI
    + [x] Undo/reset
    + [x] Change sim step
    + [x] Jump to generation
- [x] Read 2D Extended RLE patterns from Golly
    + [x] ... from clipboard
    + [ ] ... from file
- [x] Write 2D Extended RLE patterns for Golly
    + [x] ... to clipboard
    + [ ] ... to file
- [ ] Edit patterns with the mouse
- [ ] Begin work on custom rules

## Long-term to-do list

- [x] Render 2D
- [ ] Basic editing in 2D
- [ ] Basic custom rules
- [ ] Render 3D
- [ ] Basic editing in 3D
- [ ] GUI refactor

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
    + [ ] Beyond ±2^63 using `BigInt`s (probably ~~not happening for a while~~ [happening very soon](https://github.com/HactarCE/NDCell/tree/generic_ndvec))
- [ ] Bounded (finite)
- [ ] Partially bounded (e.g. tube)
- [ ] Edge conditions
    + Loop (e.g. torus)
    + Loop with offset (e.g. twisted torus)
    + Flip (e.g. Möbius loop)

### UI

- [x] "Root" window that can toggle other windows

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
    + [x] in 2D
    + [ ] in 3D
- [ ] Toggle/cycle cell state
    + [ ] in 1D
    + [ ] in 2D
    + [ ] in 3D
- [ ] More advanced editing (?)

## Possible future optimizations

- Simulation
    + [ ] Render and simulate asynchronously
    + [ ] Multithread HashLife
