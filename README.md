# NDCell [![Tests badge]][Tests link] [![Builds badge]][Builds link] [![Discord badge]][Discord link]

[Discord badge]: https://img.shields.io/discord/560924453245288459.svg?colorB=7289DA "Discord server invite"
[Discord link]: https://discord.gg/vdJwHQF
[Tests badge]: https://github.com/HactarCE/NDCell/workflows/Run%20tests/badge.svg "Test results"
[Tests link]: https://github.com/HactarCE/NDCell/actions?query=workflow%3A%22Run+tests%22
[Builds badge]: https://github.com/HactarCE/NDCell/workflows/Build%20latest/badge.svg "Download latest build"
[Builds link]: https://github.com/HactarCE/NDCell/actions?query=workflow%3A%22Build+latest%22

_Any number of dimensions_  
_Any neighborhood range_  
_Any computable transition function_

**Simulate _any_ cellular automaton**

An N-dimensional cellular automaton simulation program. Maybe. Someday.

## Downloads

Builds are automatically generated for every push to the `master` branch. **To get the latest build, click [here][Builds link] and click on the first run with a green check mark, which will contain builds for Windows, Linux, and macOS.**

On Linux and macOS, the file might not be executable by default -- to fix this, open a terminal in the same folder as the `ndcell` file and run `chmod a+x ndcell`.

## Short-term to-do list

- [x] Begin work on custom rules
    + See [`dev/lang/README.md`](https://github.com/HactarCE/NDCell/blob/dev/lang/README.md#short-term-to-do-list)
- [ ] Refactor imports to use preludes (maybe?)

## Long-term to-do list

- [x] Render 2D
- [x] Basic editing in 2D
- [ ] Basic custom rules
- [ ] Render 3D
- [ ] Basic editing in 3D

## Implementation status

### Simulation

#### Rules

- [ ] Custom rules

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
    + [x] Beyond ±2^63 using `BigInt`s
- [ ] Bounded (finite)
- [ ] Partially bounded (e.g. tube)
- [ ] Edge conditions
    + Loop (e.g. torus)
    + Loop with offset (e.g. twisted torus)
    + Flip (e.g. Möbius loop)

### UI

- [x] "Root" window that can toggle other windows
- [ ] Breakpoints
    + [ ] ... at generation X
    + [ ] ... when given cell is nonzero
- [ ] Info when hovering over a cell
    + [ ] show neighborhood
    + [ ] flash next state (maybe when certain key is held)
    + [ ] sidebar
        * [ ] display ID
        * [ ] display tags
        * [ ] display custom info (`@hoverinfo`)

### Grid display

- [ ] A 1D grid
    + [ ] as "barcode"
    + [ ] as squares
    + [ ] as 2D spacetime
- [x] A 2D grid
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
- [x] Toggle/cycle cell state
    + [ ] in 1D
    + [x] in 2D
    + [ ] in 3D
- [ ] More advanced editing (?)

### Command line interface

- [ ] CLI argument parsing using [`clap`](https://docs.rs/clap/2.33.0/clap/)

## Possible future improvements/optimizations

- Simulation
    - [ ] Generalized associative cache for nodes
        + Simulation futures, population, etc.
    + Minimize `BigInt` allocations
        * [ ] Compute population asynchronously
        * [ ] Precompute HashLife time splits
    + [ ] Garbage-collection / memory limit
    + [ ] Use fixed-size arrays instead of `Vec<NdTreeBranch<...>>`; there are two ways to implement this:
        1. As an associated type of `Dim` (needs [GATs](https://github.com/rust-lang/rust/issues/44265))
        2. Using a const generic for array length (needs [const generics](https://github.com/rust-lang/rust/issues/44580))
