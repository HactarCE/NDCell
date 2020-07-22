*****
Types
*****

Note that all types except patterns_ in NDCA have `value semantics`__, which means that modifying a value in one variable does not have an effect on any other variables.

__ https://en.wikipedia.org/wiki/Value_semantics

Basic types
===========

.. _integer:
.. _integers:

Integer
-------

:ref:`integer-methods`

Integers are represented using 64-bit signed two's complement. This means the minimum value is ``-9223372036854775808`` and the maximum value is ``9223372036854775807``.

Integers are also used to represent booleans values, like in the C programming language; ``0`` is "falsey" and any other number (generally ``1``) is "truthy." See [Boolean logic](booleans.md) for more info.

Integers are written as a sequence of digits without a leading zero but with an optional ``+`` or ``-`` at the beginning. Examples: ``0``, ``-1``, ``42``, ``+6``, ``-32768``.

NOTE: In the future, hexadecimal and/or binary literals may be supported.

.. _cell
.. _cells

Cell
----

:ref:`cell-methods`

Cells are represented using unsigned 8-bit integers holding ID of the cell's state. This means the minimum value is ``0`` and the maximum value is ``255``, so an automaton cannot have more than 256 states. Cells values are always within the range of valid cell states in a cellular automaton. For example, an automaton with ``10`` states has a maximum cell state ID of ``9``.

Single cell states are written using the ``#`` operator followed by a number. Examples: ``#0``, ``#1``, ``#42``. To use the value of a variable or expression instead of a literal integer, surround the expression in parentheses: ``#(my_variable)`` or ``#(10 + 5)``.

### Cell operations

Cells are automatically converted to [cell filters][cell filter] when used with any set operator

### Cell comparisons

Cells support the comparison operators ``==`` and ``!=``, which compare the IDs.

.. _vector
.. _vectors

Vector
------

:ref:`vector-methods`

A vector is a sequence of [integers][integer] of a fixed length. Each integer in a vector is called a component. The first one is the X component at index 0; the second is the Y component at index 1; etc. ([Zero-based indexing](https://en.wikipedia.org/wiki/Zero-based_numbering) is common in programming languages.)

Vectors are written as a list of integers separated by commas surrounded by square brackets. For example, `[3, -1, 0]` is a vector of length `3` with X component `3`, Y component `-1`, Z component `0`. Vectors can also be written using the builtin [`vec()`] function and its variants.

Vector lengths
^^^^^^^^^^^^^^

A vector must have at least one component, and must not have more than 256 components.

Vectors with different lengths are different types, and are not interchangeable. A vector can be converted to one of a different length implicitly (such as by some [operators]) or using the builtin [`vec()`] function and its variants.

When a vector is converted to have a shorter length, it is truncated and the extra components are thrown away. For example, `vec2([10, 20, 30, 40])` results in `[10, 20]`.

When a vector is converted to have a longer length, it is extended with zeros. For example, `vec4([10, 20])` results in `[10, 20, 0, 0]`.

### Vector arithmetic

Vectors support all the same arithmetic and bitwise operations as [integers][integer] by applying them componentwise. For example, `[1, 2, 3] + [10, 20, 30]` results in `[11, 22, 33]`.

For most operations, when an operation is applied between vectors of different lengths, the shorter vector is first extended using `0`. For example, `[1, 2] + [10, 20, 30]` results in `[11, 22, 30]`. For multiplication (`*`) and bitwise AND (`&`), however, the longer vector is truncated to the length of the shorter one, since the extra components would be zero anyway. So `[1, 2, 3] * [1, 2]` results in `[1, 4]`, **not** `[1, 4, 0]`.

### Vector comparisons

Vectors support all the same comparisons as [integers][integer], by applying them componentwise. When comparing vectors, the shorter vector is first extended using `0`. A comparison between vectors compares all components, and is true only if that comparison is true for all components. For example `[-1, 0] < [0, 1]` is true because `-1 < 1` and `0 < 1` are both true. `[-1, 1] < [0, 1]`, however, is false because `0 < 0` is false.

.. _pattern
.. _patterns

Pattern
-------

.. _filter-type
.. _filter-types

Filter types
============

.. _range
.. _ranges

Range
-----

.. _rectangle
.. _rectangles

Rectangle
---------

.. _cell-filter
.. _cell-filters

Cell filter
-----------

.. _pattern-filter
.. _pattern-filters

Pattern filter
--------------

.. _other-type
.. _other-types

Other types
===========

.. _tag
.. _tags

Tag
---

.. _void

Void
----

The void type is an implementation detail that will probably be removed in a future version. Ignore it for now.
