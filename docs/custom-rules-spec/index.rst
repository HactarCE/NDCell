******************
NDCA specification
******************

NDCA is the `domain-specific`__ programming language for custom rules used by NDCell. NDCA files have the ``.ndca`` extension, which stands for **N**-**D**\ imensional **C**\ ellular **A**\ utomaton.

__ https://en.wikipedia.org/wiki/Domain-specific_language

Rationale
=========

Most existing cellular automaton simulation programs that support custom rules either require them defined in verbose languages like C or Java or in as a list of transitions, which is limiting and hard to read. A custom programming language provides the power of math, logic, and variables with utilities unique to cellular automata, like first-class support for symmetries and masked N-dimensional arrays.

The original drafts for NDCell used `Lua`__ for defining custom rules, but I changed my mind for a number of reasons.

__ https://www.lua.org/

- **Speed** — Lua can be JIT-compiled, but it still has dynamic typing which costs speed compared to a statically typed JIT-compiled language.
- **Less bloat** — Though Lua is very small, it has many features that are not helpful in most cellular automaton, such as tables, metatables, double-precision floating point numbers, etc.
- **Guarantees** — NDCA offers useful guarantees by disallowing dynamic memory allocation and nondeterministic behavior, which improve safety and reliability.
- **Custom features** — Types like :ref:`vectors <vectors>` and :ref:`cell patterns <patterns>` are first-class in NDCA, and there is custom syntax for handling symmetries, groups of cell states, and numerous other features that would be awkward to emulate in a general-purpose programming language.
