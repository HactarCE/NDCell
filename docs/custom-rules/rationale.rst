*********
Rationale
*********

Why make a new programming language?
====================================

Most existing cellular automaton simulation programs that support custom rules either require them defined in existing programming languages like C or Java, which tend to be verbose, or as a list of transitions, which is limiting and hard to read. A custom programming language provides the power of math, logic, and variables with utilities unique to cellular automata, like first-class support for spatial symmetry and masked N-dimensional arrays.

The original drafts for NDCell used `Lua`__ for defining custom rules, but I decided to create a custom programming language for several reasons:

__ https://www.lua.org/

- **Speed** — Lua can be JIT-compiled, but it still has dynamic typing which costs speed compared to a statically typed JIT-compiled language.
- **Less bloat** — Though Lua is very small, it has many features that are not helpful in most cellular automata, such as tables, metatables, double-precision floating point numbers, etc.
- **Guarantees** — NDCA offers useful guarantees by disallowing dynamic memory allocation and nondeterministic behavior, which improve safety and reliability.
- **Custom features** — Types like vectors and cell configurations are first-class in NDCA, and there is custom syntax for handling symmetries, sets, and numerous other features that would be awkward to emulate in a general-purpose programming language.

NDCA takes inspiration from the following sources:

- **Rust** — basic syntax and static typing with inference
- **GLSL** — first-class vectors and user code designed to run in parallel, processing a vast amount of data in small chunks
- **Golly rule tables** — symmetry, ``@directive`` syntax, and pattern matching
- **Java** — ``>>>`` logical right-shift operator

.. _set-contents-rationale:

Why are some collections with different contents considered different types?
============================================================================

Values of :data:`IntSet`, :data:`VecSet`, :data:`PatternFilter`, and :data:`String` with different contents are considered different types for the purpose of variable assignment. This enables greater performance and reliability for the following reasons:

- Operations on these types could produce arbitrarily complex values, which would require `dynamic memory allocation`__ inside the transition function, which is complex and slow and introduces the possibility of memory leaks
- Operations on these types such as iteration and pattern maching are computed at compile-time, which enables much better optimization

__ https://en.wikipedia.org/wiki/Memory_management#HEAP
