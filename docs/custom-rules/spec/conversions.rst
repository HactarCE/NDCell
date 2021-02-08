.. include:: <isonum.txt>

.. _conversions:

***********
Conversions
***********

Some types in NDCell are implicitly converted (coerced) to other types when used with various operators or passed to functions.

.. _subtype-coercion:

Subtype coercion
================

Some types are `subtypes`__ of other types; this is implemented by coercing the subtype to the supertype when necessary. For example, a :data:`Cell` is implicitly converted to a :data:`CellSet` when used where a :data:`CellSet` is required. Here are the rules for subtype coercion:

__ https://en.wikipedia.org/wiki/Subtyping

- The :data:`CellSet` coerced from a :data:`Cell` contains only that cell state
- The :data:`CellSet` coerced from a :data:`Tag` contains all cell states with a :ref:`truthy <boolean-conversion>` value for that tag
- The :data:`PatternFilter` coerced from a :data:`Pattern` accepts only that pattern

.. _boolean-conversion:

Boolean conversion
==================

Values of some types can be converted to a boolean, which is represented using an :data:`Int`. This can happen implicitly (when used in a place where a boolean is required) or explicitly (using :func:`bool`).

- An :data:`Int` is truthy if it is not equal to ``0``.
- A :data:`Cell` is truthy if it is not equal to ``#0``.
- A :data:`Vec` is truthy if any of its components is not equal to ``0``.
- A :data:`Pattern` is truthy if any of its cells is not equal to ``#0``.

"Truthy" values become :data:`TRUE` (``1``) and "falsey" values (anything not truthy) become :data:`FALSE` (``0``).

.. _vector-vector-conversion:

Vector to vector conversion
===========================

A :data:`Vec` of one length can be converted to a :data:`Vec` of a different length. This can happen implicitly (when used in a place where a :data:`Vec` of a different length is required) or explicitly (using :func:`vec`).

- If the new length is shorter than the original length, the vector is truncated and extra components are removed.

  - Example: ``vec2([10, 20, 30, 40])`` |rarr| ``[10, 20]``

- If the new length is longer than the original length, the vector is extended with zeros.

  - Example: ``vec4([10, 20])`` |rarr| ``[10, 20, 0, 0]``

.. _integer-vector-conversion:

Integer to vector conversion
============================

An :data:`Int` can be converted to a :data:`Vec` of any length. This can happen implicitly (when used in a place where a :data:`Vec` is required) or explicitly (using :func:`vec`).

A new vector is constructed with the value of the original integer for each component.

- Example: ``vec3(-5)`` |rarr| ``[-5, -5, -5]``
