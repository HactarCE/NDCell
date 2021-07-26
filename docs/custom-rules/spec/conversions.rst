.. include:: <isonum.txt>

.. _conversions:

***********
Conversions
***********

Some types in NDCell are implicitly converted (coerced) to other types when used with various operators or passed to functions.

.. _subtype-coercion:

Subtype coercion
================

Some types are `subtypes`__ of other types; this is implemented by coercing the subtype to the supertype when necessary. For example, a :data:`Cell` is implicitly converted to a :data:`CellSet` when used anywhere that a :data:`CellSet` is expected. Here are the rules for subtype coercion:

__ https://en.wikipedia.org/wiki/Subtyping

- The :data:`CellSet` coerced from a :data:`Cell` contains only that one cell state
- The :data:`CellSet` coerced from a :data:`Tag` contains all cell states with a :ref:`truthy <boolean-conversion>` value for that tag
- The :data:`Pattern` coerced from an :data:`Array` accepts only that one array.
- The :data:`IntegerSet`, :data:`CellSet`, or :data:`VectorSet` coerced from an :data:`EmptySet` contains no elements.

Readers familiar with C++ might find it useful to think of these coercions as implicit converting constructors.

.. _boolean-conversion:

Boolean conversion
==================

Values of some types can be converted to a boolean, which is represented using an :data:`Integer`. This can happen implicitly (when used in a place where a boolean is required) or explicitly (using :func:`bool`).

- An :data:`Integer` is truthy if it is not equal to ``0``.
- A :data:`Cell` is truthy if it is not equal to ``#0``.
- A :data:`Vector` is truthy if any of its components is not equal to ``0``.
- A :data:`Pattern` is truthy if any of its cells is not equal to ``#0``.

"Truthy" values become :data:`TRUE` (``1``) and "falsey" values (anything not truthy) become :data:`FALSE` (``0``).

.. _vector-vector-conversion:

Vector to vector conversion
===========================

A :data:`Vector` of one length can be converted to a :data:`Vector` of a different length. This can happen implicitly (when used in a place where a :data:`Vector` of a different length is required) or explicitly (using :func:`vec`).

- If the new length is shorter than the original length, the vector is truncated and extra components are removed.

  - Example: ``vec2([10, 20, 30, 40])`` |rarr| ``[10, 20]``

- If the new length is longer than the original length, the vector is extended with zeros.

  - Example: ``vec4([10, 20])`` |rarr| ``[10, 20, 0, 0]``

.. _integer-vector-conversion:

Integer to vector conversion
============================

An :data:`Integer` can be converted to a :data:`Vector` of any length. This can happen implicitly (when used in a place where a :data:`Vector` is required) or explicitly (using :func:`vec`).

A new vector is constructed with the value of the original integer for each component.

- Example: ``vec3(-5)`` |rarr| ``[-5, -5, -5]``
