*********
Operators
*********

Besides the ``is`` operator, both `arguments`__ to any binary (two-input) operator must be of the same type, or one type must be able to implicitly convert to the other. See :ref:`conversions` for more details. Some additional rules apply for operators:

__ https://en.wikipedia.org/wiki/Argument_of_a_function

- For the binary operators ``+``, ``-``, ``|``, ``^``, ``<<``, ``>>>``, and ``>>``, the shorter vector is extended and the result is the length of the longer vector.
- For the binary operators ``*``, ``/``, ``%``, ``**``, and ``&``, the longer vector is truncated and the result is the length of the shorter vector. (This is because extending the shorter vector with ``0`` would not produce useful output.)
- Arguments to a boolean operator

- Integers will implicitly convert to vectors of any length. See [Integer to vector conversion](#integer-to-vector-conversion).
- Cells will implicitly convert to cell filters. See [Cell to cell filter conversion](#cell-to-cell-filter-conversion).
- Vectors, cells, and patterns will implicitly convert to booleans. (Note that integers are already booleans.)
  - A vector is truthy if any component is nonzero.
  - A cell is truthy if it is not ``#0``.
  - A pattern is truthy if any cell is nonzero.

.. _arithmetic-operators:

Arithmetic operators
====================

Given two values ``a`` and ``b``:

- ``a + b`` — addition
- ``a - b`` — subtraction
- ``a * b`` — multiplication
- ``a / b`` — division (rounds toward zero)
- ``a % b`` — remainder
- ``a ** b`` — exponentiation
- ``-a`` — negation
- ``+a`` — no operation

NOTE: In the future, the behavior of ``/`` and ``%`` with negative numbers may be changed to emulate [floored or euclidean division/modulo](https://en.wikipedia.org/wiki/Modulo_operation).

All of these operations can be applied to integers and vectors.

If an operation is applied to two vectors of different lengths or applied to a vector and an integer, then both arguments are converted to the a vector of the same length:

- For ``+`` and ``-``, the length of the shorter vector is used.
-

. (See [Conversions] for more details.) For ``+`` and ``-``, the shorter vector is extended; for ``*``, ``/``, ``%``, and ``**``, the longer vector is truncated.

Overflow, underflow, or division by zero abort the simulation with an error.

[vector lengths]: Types#vector-lengths

.. _bitwise-operators:

Bitwise operators
=================

Given two values ``a`` and ``b``:

- ``a & b`` — [bitwise AND]
- ``a | b`` — [bitwise OR]
- ``a ^ b`` — [bitwise XOR]
- ``a >> b`` — [bitshift right (arithmetic/signed)][arithmetic shift]
- ``a >>> b`` — [bitshift right (logical/unsigned)][logical shift]
- ``a << b`` — [bitshift left][logical shift]
- ``~a`` — [bitwise NOT]

[bitwise AND]: https://en.wikipedia.org/wiki/Bitwise_operation#AND
[bitwise OR]: https://en.wikipedia.org/wiki/Bitwise_operation#OR
[bitwise XOR]: https://en.wikipedia.org/wiki/Bitwise_operation#XOR
[arithmetic shift]: https://en.wikipedia.org/wiki/Arithmetic_shift
[logical shift]: https://en.wikipedia.org/wiki/Logical_shift
[bitwise NOT]: https://en.wikipedia.org/wiki/Bitwise_operation#NOT

All of these operations are defined for integers and vectors.

Bitshifting by less than 0 or more than 64 aborts the simulation with an error.

.. _set-operators:

Set operators
=============

Given two values ``a`` and ``b``:

- ``a & b`` — [intersection]
- ``a | b`` — [union]
- ``a ^ b`` — [symmetric difference]
- ``~a`` — [complement]

[intersection]: https://en.wikipedia.org/wiki/Intersection_(set_theory)
[union]: https://en.wikipedia.org/wiki/Union_(set_theory)
[symmetric difference]: https://en.wikipedia.org/wiki/Symmetric_difference
[complement]: https://en.wikipedia.org/wiki/Complement_(set_theory)

Set operations are defined for cell filters. If one of these operators is applied to a cell, it is automatically converted to a cell filter that matches only that cell.

.. _comparison-operators:

Comparison operators
====================

- ``a == b`` — Does ``a`` equal ``b``?
- ``a != b`` — Does ``a`` not equal ``b``?
- ``a < b`` — Is ``a`` less than ``b``?
- ``a > b`` — Is ``a`` greater than ``b``?
- ``a <= b`` — Is ``a`` less than or equal to ``b``?
- ``a >= b`` — Is ``a`` greater than or equal to ``b``?

All of these operations are defined for integers and vectors. ``==`` and ``!=`` are defined for cell states and patterns.

.. _boolean-operators:

Boolean operators
=================

- ``a and b`` — [logical AND]
- ``a or b`` — [logical OR]
- ``a xor b`` — [logical XOR]
- ``not a`` — [logical NOT]

[logical AND]: https://en.wikipedia.org/wiki/AND_gate
[logical OR]: https://en.wikipedia.org/wiki/OR_gate
[logical XOR]: https://en.wikipedia.org/wiki/XOR_gate
[logical NOT]: https://en.wikipedia.org/wiki/Inverter_(logic_gate)

.. _range-operator:

Range operator
==============

- ``a..b``

.. _indexing:

Indexing
========

- ``a[b]``

.. _is-operator:

Membership test
===============

- ``a is b``

This operator takes a basic type for ``a`` and the corresponding filter type for ``b``. (Note that implicit type conversion rules apply)
