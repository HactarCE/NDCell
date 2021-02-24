.. include:: <isonum.txt>

*********
Operators
*********

.. note::

  This page is under construction.

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

- ``a + b`` — Addition
- ``a - b`` — Subtraction
- ``a * b`` — Multiplication
- ``a / b`` — Division (rounds toward zero)
- ``a % b`` — Remainder
- ``a ** b`` — Exponentiation
- ``-a`` — Negation
- ``+a`` — No operation

NOTE: In the future, the behavior of ``/`` and ``%`` with negative numbers may be changed to emulate `floored or euclidean division/modulo`__.

__ https://en.wikipedia.org/wiki/Modulo_operation

All of these operations can be applied to integers and vectors. Vector operations are applied componentwise.

If an operation is applied to two vectors of different lengths, one vector is cast to the length of the other before the operation is applied. (See :ref:`vector-vector-conversion`.)

- For ``+`` and ``-``, the shorter vector is cast to the length of the **longer** one
- For ``*``, ``/``, ``%``, and ``**``, the longer vector is cast to the length of the **shorter** one

If an operation is applied to a vector and an integer, in either order, then the integer is converted to a vector of the same length before the operation is applied. (See :ref:`integer-vector-conversion`.)

Overflow, underflow, division by zero, or exponentiation with a negative power cause an error. (See :ref:`errors`.)

Examples
--------

- ``2 ** 8`` |rarr| ``256``
- ``[1, 2] + [10, 20, 30]`` |rarr| ``[11, 22, 30]``
- ``[1, 2, 3] * [1, 2]`` |rarr| ``[1, 4]``
- ``12 / [2, 3, 4, 5]`` |rarr| ``[6, 4, 3, 2]``
- ``[4] % [2, 0, 1]`` |rarr| ``[0]``
- ``4 % [2, 0, 1]`` causes a division-by-zero error

.. _bitwise-operators:

Bitwise operators
=================

Given two values ``a`` and ``b``:

- ``a & b`` — `Bitwise AND <https://en.wikipedia.org/wiki/Bitwise_operation#AND>`_
- ``a | b`` — `Bitwise OR <https://en.wikipedia.org/wiki/Bitwise_operation#OR>`_
- ``a ^ b`` — `Bitwise XOR <https://en.wikipedia.org/wiki/Bitwise_operation#XOR>`_
- ``a >> b`` — `Bitshift right (arithmetic/signed) <https://en.wikipedia.org/wiki/Arithmetic_shift>`_
- ``a >>> b`` — `Bitshift right (logical/unsigned) <https://en.wikipedia.org/wiki/Logical_shift>`_
- ``a << b`` — `Bitshift left <https://en.wikipedia.org/wiki/Logical_shift>`_
- ``~a`` — `Bitwise NOT <https://en.wikipedia.org/wiki/Bitwise_operation#NOT>`_

All of these operations can be applied to integers and vectors. Vector operations are applied componentwise.

If an operation is applied to two vectors of different lengths, one vector is cast to the length of the other before the operation is applied. (See :ref:`vector-vector-conversion`.)

- For ``|``, ``^``, ``>>``, ``<<<``, and ``<<``, the shorter vector is cast to the length of the **longer** one
- For ``&``, the longer vector is cast to the length of the **shorter** one

If an operation is applied to a vector and an integer (in either order), then the integer is converted to a vector of the same length. (See :ref:`integer-vector-conversion`.)

Bitshifting by less than 0 or more than 64 causes an error. (See :ref:`errors`.)

.. _set-operators:

Set operators
=============

Given two values ``a`` and ``b``:

- ``a & b`` — `Intersection <https://en.wikipedia.org/wiki/Intersection_(set_theory)>`_
- ``a | b`` — `Union <https://en.wikipedia.org/wiki/Union_(set_theory)>`_
- ``a ^ b`` — `Symmetric difference <https://en.wikipedia.org/wiki/Symmetric_difference>`_
- ``a &~ b`` — `Relative complement of B in A <https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement>`_

All of these operations can be applied to any :ref:`set/filter type <filter-types>` or its subtypes.

.. _comparison-operators:

Comparison operators
====================

Given two values ``a`` and ``b``:

- ``a == b`` — Does ``a`` equal ``b``?
- ``a != b`` — Does ``a`` not equal ``b``?
- ``a < b`` — Is ``a`` less than ``b``?
- ``a > b`` — Is ``a`` greater than ``b``?
- ``a <= b`` — Is ``a`` less than or equal to ``b``?
- ``a >= b`` — Is ``a`` greater than or equal to ``b``?

All of these comparisons can be applied to integers and vectors. ``==`` and ``!=`` can be applied to cell states and patterns. Vector comparisons are applied componentwise.

If a comparison is applied to two vectors of different lengths, the shorter vector is to the length of the longer one before the operation is applied. (See :ref:`vector-vector-conversion`.)

If a comparison is applied to a vector and an integer (in either order), then the integer is converted to a vector of the same length. (See :ref:`integer-vector-conversion`.)

A ``!=`` comparison between two vectors results in :data:`TRUE` if **any** corresponding components of the two vectors are unequal. All other comparisons between two vectors result in :data:`TRUE` if only if the comparison is :data:`TRUE` for **all** corresponding components of the two vectors. Examples:

- ``[1, 2] == [1, 2, 0]`` |rarr| :data:`TRUE`
- ``[1, 2] == [1, 2, 3]`` |rarr| :data:`FALSE` because ``0 == 3`` |rarr| :data:`FALSE`
- ``[1, 2] != [1, 2, 3]`` |rarr| :data:`TRUE` because ``0 != 3`` |rarr| :data:`TRUE`
- ``[-1, 2] < [0, 4]`` |rarr| :data:`TRUE` because ``-1 < 0`` |rarr| :data:`TRUE` and ``2 < 4`` |rarr| :data:`TRUE`
- ``[-1, 2] < [0, 1]`` |rarr| :data:`FALSE` because ``2 < 1`` |rarr| :data:`FALSE`

.. _boolean-operators:

Boolean operators
=================

- ``a and b`` — `Logical AND <https://en.wikipedia.org/wiki/AND_gate>`_
- ``a or b`` — `Logical OR <https://en.wikipedia.org/wiki/OR_gate>`_
- ``a xor b`` — `Logical XOR <https://en.wikipedia.org/wiki/XOR_gate>`_
- ``not a`` — `Logical NOT <https://en.wikipedia.org/wiki/Inverter_(logic_gate)>`_

.. _range-operator:

Range operator
==============

- ``a..b``

.. _vector-indexing:

Vector indexing
===============

- ``v[n]``

Indexing a vector ``v`` by an integer ``n`` results in the ``n``-th component of ``v``. The X component has index ``0``, the Y component has index ``1``, etc. Indexing with a value ``n`` that is not between ``0`` and ``v.len - 1`` (inclusive) causes an error. (See :ref:`errors`.)

.. _is-operator:

Filter test
===========

- ``a is b``

This operator takes a basic type for ``a`` and the corresponding filter type for ``b``.
