# NDCell [![Release badge]][Release link] [![Tests badge]][Tests link] [![Dependencies badge]][Dependencies status link] [![Discord badge]][Discord link]

[Dependencies badge]: https://deps.rs/repo/github/HactarCE/NDCell/status.svg "Dependencies status"
[Dependencies status link]: https://deps.rs/repo/github/HactarCE/NDCell "GitHub release (latest)"
[Discord badge]: https://img.shields.io/discord/560924453245288459.svg?colorB=7289DA "Discord server invite"
[Discord link]: https://discord.gg/vdJwHQF
[Release badge]: https://img.shields.io/github/v/release/HactarCE/NDCell
[Release link]: https://github.com/HactarCE/NDCell/releases/latest
[Tests badge]: https://img.shields.io/github/workflow/status/HactarCE/NDCell/test?label=test "Test results"
[Tests link]: https://github.com/HactarCE/NDCell/actions?query=workflow%3A%22test%22

[NDCA docs]: https://ndcell.rtfd.io/

<img src="https://raw.githubusercontent.com/HactarCE/NDCell/master/docs/img/ndcell_icon.svg?sanitize=true" alt="NDCell logo" width="150" align="right">

_Any number of dimensions_  
_Any neighborhood range_  
_Any computable transition function_

**Simulate _any_ deterministic cellular automaton**

An N-dimensional cellular automaton simulation program. Maybe. Someday.

## Downloads

Download the **[latest release][Release link]**.

On Linux and macOS, the file might not be executable by default -- to fix this, open a terminal in the same folder as the `ndcell` file and run `chmod a+x ndcell`.

To build NDCell yourself, see [BUILDING.md](BUILDING.md).

## Screenshots

Click to view full resolution / play video.

<img src="https://i.imgur.com/vRMLNYC.png" alt="Gosper's Glider Gun simulated for 57 generations" width="256" />
<img src="https://i.imgur.com/uKiOxqy.png" alt="Catacryst simulated for 7.9 million generations" width="256" />
<img src="https://i.imgur.com/NAxRaYd.png" alt="WireWorld primes calculator simulated for 2.9 million generations, displaying the number 23" width="256" />
<a href="https://i.imgur.com/xAhILIO.mp4"><img src="https://i.imgur.com/aWGsV9Y.png" alt="3D WireWorld XOR gate supporting 3-micron signals" width="256" /></a>

## What works right now

See the [Changelog](CHANGELOG.md) for the latest list of features.

## What's planned for the future

In roughly descending order of priority/likelihood:

- **[Everything on the short-term to-do list](TODO.md)**, including **3D rendering/simulation**
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

## Contributing

NDCell is currently under very rapid development, and a lot of things are in flux. Bug reports/fixes and minor feature requests are welcome, either via [Discord][Discord link] or the [issues tracker](https://github.com/HactarCE/NDCell/issues), but the project is currently not in a position to accept larger contributions. Ideas and feedback are always welcome on the Discord server.

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  <https://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <https://opensource.org/licenses/MIT>)

at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
