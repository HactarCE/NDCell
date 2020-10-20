# NDCell [![Tests badge]][Tests link] [![Builds badge]][Builds link] [![Discord badge]][Discord link]

[Discord badge]: https://img.shields.io/discord/560924453245288459.svg?colorB=7289DA "Discord server invite"
[Discord link]: https://discord.gg/vdJwHQF
[Tests badge]: https://github.com/HactarCE/NDCell/workflows/Run%20tests/badge.svg "Test results"
[Tests link]: https://github.com/HactarCE/NDCell/actions?query=workflow%3A%22Run-tests%22
[Builds badge]: https://github.com/HactarCE/NDCell/workflows/Build%20latest/badge.svg "Download latest build"
[Builds link]: https://github.com/HactarCE/NDCell/actions?query=workflow%3A%22Build-latest%22

[NDCA docs]: https://ndcell.rtfd.io/

_Any number of dimensions_  
_Any neighborhood range_  
_Any computable transition function_

**Simulate _any_ cellular automaton**

An N-dimensional cellular automaton simulation program. Maybe. Someday.

## Screenshots

<img src="https://i.imgur.com/vRMLNYC.png" alt="Gosper's Glider Gun simulated for 57 generations" width="512" />
<img src="https://i.imgur.com/uKiOxqy.png" alt="Catacryst simulated for almost 8 million generations" width="512" />

## Downloads

Builds are automatically generated for every push to the `master` branch. **To get the latest build, click [here][Builds link] and click on the first run with a green check mark, which will contain builds for Windows, Linux, and macOS.** You must be logged into GitHub for the link to be available. If you do not want to log into GitHub or you're having trouble with the latest build, join [the Discord server][Discord link] and ask for help.

On Linux and macOS, the file might not be executable by default -- to fix this, open a terminal in the same folder as the `ndcell` file and run `chmod a+x ndcell`.

To build NDCell yourself, see [BUILDING.md](BUILDING.md).

## To-do list

- [x] Render 2D
- [x] Basic editing in 2D
- [x] Basic custom rules
  - [x] Implementation
  - [ ] [Documentation][NDCA docs]
- [ ] Improve performance
  - [x] New ND-tree node cache
    - [x] Garbage collection
  - [x] Lazy population computation
  - [ ] Multithreaded simulation
  - [x] Reduce `BigInt` allocations
    - [x] Precompute HashLife time splits
  - [ ] Rewrite RLE export/import
  - [ ] Macrocell export/import
- [ ] Render 3D
  - [ ] Cubes
  - [ ] Lighting
  - [ ] Gridlines
  - [ ] Improve performance
    - [ ] Limit visibility / implement render cells
    - [ ] Reduce detail on distant octree nodes
    - [ ] Hide occluded faces
    - [ ] Merge adjacent faces
- [ ] Basic editing in 3D
- [ ] Improve UI
  - [ ] Better error reporting
    - [ ] RLE loading
    - [ ] Compile error from custom rule
    - [ ] Runtime error from custom rule
  - [ ] Rectangular selection
  - [ ] Copy/paste selection
  - [ ] Rotate/reflect selection
  - [ ] Simulate in selection
  - [ ] Save/load patterns
  - [ ] Select rule
  - [ ] Preferences
    - [ ] Tweak rendering parameters
    - [ ] Customize default colors
    - [ ] Customize gridlines
    - [ ] Customize key bindings
    - [ ] Customize mouse bindings
- [ ] More custom rules features
  - [ ] Finish documentation
  - [ ] More methods (`.count`)
  - [ ] More cell states
  - [ ] Pattern masks
  - [ ] Pattern matching
  - [ ] Basic symmetries
  - [ ] Spacetime residues

## What works right now

- **Edit and simulate Conway's Game of Life on an infinite 2D grid using HashLife**
- **Custom rules** written in NDCA, a custom JIT-compiled programming language. Documentation in progress at [ndcell.rtfd.io][NDCA docs]. ([Demo video][custom-rules-demo])
- **Undo history**
  - Undo with <kbd>Ctrl</kbd>+<kbd>Z</kbd>
  - Redo with <kbd>Ctrl</kbd>+<kbd>Y</kbd> or <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Z</kbd>
- **Import/export**
  - Copy RLE with <kbd>Ctrl</kbd>+<kbd>C</kbd>
  - Paste RLE with <kbd>Ctrl</kbd>+<kbd>V</kbd>
- **Freeform drawing**
  - Toggle cells with left click
  - Select a cell state using <kbd>0</kbd>-<kbd>9</kbd>
  - Cycle selected cell state using <kbd>[</kbd>/<kbd>]</kbd>
- **Rectangular selection** ([Demo video][selections-demo])
  - Select with left click while holding <kbd>Ctrl</kbd>
  - Resize the selection with left click while holding <kbd>Ctrl</kbd>+<kbd>Shift</kbd>, or by dragging the edges of the selection
- **Navigation**
  - Pan with arrow keys, <kbd>W</kbd>/<kbd>A</kbd>/<kbd>S</kbd>/<kbd>D</kbd>, or right mouse button (hold <kbd>Shift</kbd> to go faster)
  - Zoom with scroll wheel or <kbd>Q</kbd>/<kbd>Z</kbd> (hold <kbd>Shift</kbd> to go faster)
- **Simulation**
  - Step 1 generation with <kbd>Space</kbd>
  - Step N generations with <kbd>Tab</kbd>
  - Run continuously with <kbd>Enter</kbd>
  - Set step size
    - Double/halve step size with <kbd>+</kbd>/<kbd>-</kbd>
  - Set breakpoint
- **3D rendering**
  - Partially working ([Demo video][3d-demo])

[custom-rules-demo]: https://streamable.com/7yawnq
[selections-demo]: https://streamable.com/taei4j
[3d-demo]: https://streamable.com/7yp8ml

## What's planned for the future

In roughly descending order of priority/likelihood:

- **Arbitrary range and neighborhood** within a reasonable limit.
- **Custom symmetries** with different cell states for each orientation. Any symmetry can be used in part or all of the transition function.
- **Custom colors and icons** with several built-in presets and primitives.
- **3D simulation and editing**
- **Custom 3D models** with several built-in presets and primitives.
- **Anywhere from 1 to 6 dimensions** - Only a 2D or 3D slice of spacetime will be shown at any time; the position of the slice along the hidden axes will be adjustable.
- **Command-line interface** for use in scripts and other automated tools.
- **Lua scripting** - Still working out the details -- I'll update this when I have a more concrete plan.
- **Spacetime residue** - Rules will be able to define several arbitrary "spacetime residues" (linear combination of spatial and temporal positions, modulo some value) available for use in the transition function, within some reasonable limit. This means a rule like [Busy Boxes] would only need 2 states instead of 7.
- **Block CA** such as Margolus - This would be possible anyway with the spacetime residues above (using `x + time mod 2` and `y + time mod 2`), but native support would improve ergonomics.
- **Custom regular tilings** - Only those that are an affine transformation away from square/cubic. (Hexagonal and triangular neighborhoods qualify!)
- **Reversible CA** - "Go back in time" in a reversible CA, if an inverse transition function is defined.
- **Grid topology** - Each axis will be able to be infinite, half-infinite, finite, looped (torus), looped with an offset (twisted torus), or flipped (Möbius loop),
- **Infinite agars** and **true support for "B0" rules**

[Busy Boxes]: http://busyboxes.org/

## What's NOT planned for the future

These are things that NDCell will probably never support, in rough order from least likely to most likely:

- **More than 256 states** - Original plans for NDCell included up to 2^64 states, but this particular generalization has proved much harder to support than arbitrary range or number of dimensions without significantly harming memory usage and speed for automata with 256 states or fewer, which comprise the vast majority of CA. Hopefully other features like spacetime residue will be able to curb the number of states required for most automata.
- **[LtL] and other "continuous" CA** - Although NDCell can simulate LargerThanLife with smaller neighborhoods, continuous CA are better simulated using [GPGPU]. [Ready] is a fantastic simulator for such automata written by the Golly Gang.
- **Nondeterministic CA** - Nondeterministic CA are incompatible with [HashLife], which NDCell relies on for efficient simulation. It's possible that NDCell will support non-hashing simulation in the future, but nondeterministic CA would still require significant work to make usable.

[LtL]: http://golly.sourceforge.net/Help/Algorithms/Larger_than_Life.html
[GPGPU]: https://en.wikipedia.org/wiki/General-purpose_computing_on_graphics_processing_units
[Ready]: https://github.com/GollyGang/ready/
[HashLife]: https://en.wikipedia.org/wiki/Hashlife
