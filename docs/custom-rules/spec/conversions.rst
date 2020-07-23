***********
Conversions
***********

.. include:: <isonum.txt>

Some types in NDCell are implicitly converted to other types when used with various operators or passed to functions.

.. _vector-vector-conversion:

Vector to vector conversion
===========================

A vectors of one length can be converted to a vector of a different length. This can happen implicitly (when used in a place where a vector of a different length is required) or explicitly (using :func:`vec`).

- If the new length is shorter than the original length, the vector is truncated and extra components are removed.

  - Example: ``vec2([10, 20, 30, 40])`` |rarr| ``[10, 20]``

- If the new length is longer than the original length, the vector is extended with zeros.

  - Example: ``vec4([10, 20])`` |rarr| ``[10, 20, 0, 0]``

.. _integer-vector-conversion:

Integer to vector conversion
============================

An integer can be converted to a vector of any length. This can happen implicitly (when used in a place where a vector is required) or explicitly (using :func:`vec`).

A new vector is constructed with the value of the original integer for each component.

- Example: ``vec3(-5)`` |rarr| ``[-5, -5, -5]``

.. _cell-to-cell-filter-conversion:

Cell to cell filter conversion
===============================

.. _boolean-conversion:

Boolean conversion
==================

Any :ref:`basic type <basic-types>` can be converted to a boolean. This can happen implicitly (when used in a place where a boolean is required) or explicitly (using :func:`bool`).

- An integer is truthy if it is not equal to ``0``.
- A vector is truthy if any of its components is not equal to ``0``.
- A cell is truthy if it is not equal to ``#0``.
- A pattern is truthy if any of its cells is not equal to ``#0``.

"Truthy" values become :data:`TRUE` (``1``) and "falsey" values become :data:`FALSE` (``0``).
