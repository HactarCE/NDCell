.. _types:

*****
Types
*****

Overview
========

- There are two important primitive types: :data:`Int` and :data:`Cell`
- Each of these two primitive types has a collection type: :data:`Vec` (collection of :data:`Int`) and :data:`Pattern` (collection of :data:`Cell`)
- Each of these four types has a corresponding set/filter type: :data:`IntSet`, :data:`CellSet`, :data:`VecSet`, and :data:`PatternFilter`

  - :data:`Cell` is a subtype [#f1]_ of :data:`CellSet`
  - :data:`Pattern` is a subtype of :data:`PatternFilter`

- There are two other primitive types: :data:`Tag` and :data:`String`

  - :data:`Tag` is also a subtype of :data:`CellSet`

.. [#f1] I.e. anywhere that a :data:`CellSet` is required, a :data:`Cell` is accepted as well. All operations on a :data:`CellSet` are also allowed on a :data:`Cell`. See https://en.wikipedia.org/wiki/Subtyping.

See :ref:`subtype-coercion` for more about subtypes.

See :ref:`variable-types` regarding how variables use the type system.

.. _primitive-types:

Primitive types
===============

.. data:: Int

  :status: Fully implemented
  :methods: :ref:`int-methods`
  :operators: :ref:`arithmetic-operators`, :ref:`bitwise-operators`, :ref:`comparison-operators`

  An integer, represented using a 64-bit signed two's complement integer. This means the minimum value is ``-9223372036854775808`` and the maximum value is ``9223372036854775807``.

  Boolean values are represented using integers. (See :ref:`boolean-conversion`.)

  An integer literal consists of a sequence of digits without a leading zero but with an optional ``+`` or ``-`` at the beginning. Examples:

    - ``0``
    - ``-1``
    - ``42``
    - ``+6``
    - ``-32768``

.. data:: Cell

  :status: Fully implemented
  :methods: :ref:`cell-methods`
  :operators: :ref:`set-operators`, :ref:`comparison-operators` (``==`` and ``!=`` only)
  :subtype of: :data:`CellSet`

  A cell state, represented using an 8-bit unsigned integer. This means the minimum value is ``0`` and the maximum value is ``255``, so an automaton cannot have more than 256 states. :data:`Cell` values are always within the range of valid cell states in a cellular automaton. For example, an automaton with 10 states has a maximum cell state ID of ``9``.

  :data:`Cell` is a subtype of :data:`CellSet`. When used in place of a :data:`CellSet`, a :data:`Cell` represents a set containing only the one cell state.

  A :data:`Cell` literal consists of the ``#`` operator followed by the cell state ID. Examples:

    - ``#0``
    - ``#1``
    - ``#42``

  A :data:`Cell` literal may use an arbitrary integer expression for the cell state ID by surrounding the expression in parentheses. Examples:

    - ``#(my_variable)``
    - ``#(x + 5)``

.. data:: Tag

  :status: Not yet implemented

  This type's design is still a work in progress.

.. data:: String

  :status: Partially implemented

  Different :data:`String` values are different types, and therefore cannot be stored in the same variable. (See :ref:`set-contents-rationale`)

  This type's design is still a work in progress.

.. _collection-types:

Collection types
================

.. data:: Vec

  :status: Fully implemented
  :methods: :ref:`vec-methods`
  :operators: :ref:`arithmetic-operators`, :ref:`bitwise-operators`, :ref:`comparison-operators`, :ref:`vector-indexing`

  A vector, represented using a fixed-length array of :data:`Int` values. Each :data:`Int` value is a component of the :data:`Vec`, and the number of components is the length of the :data:`Vec`. The length of a :data:`Vec` must be between 1 and 256 (inclusive). :data:`Vec` values of different lengths are different types, and therefore cannot be stored in the same variable.

  The first component of a :data:`Vec` is the X component at index 0; the second is the Y component at index 1; etc.

  A :data:`Vec` literal consists of a list of integer expressions separated by commas surrounded by square brackets. Examples:

  - ``[3, -1, 0]`` is a :data:`Vec` of length ``3`` with X component ``3``, Y component ``-1``, Z component ``0``
  - ``[6]`` is a :data:`Vec` of length ``1`` with X component ``6``
  - ``[a, b]`` is a :data:`Vec` of length ``2`` with X compoment ``a`` and Y component ``b``, given ``a`` and ``b`` are integers

  A :data:`Vec` literal may contain other vectors, which are concatenated to produce the result. Examples:

  - ``[v1, -3, v2]`` is a :data:`Vec` constructed by concatenating ``v1``, ``[-3]``, and ``v2``

  A :data:`Vec` can also be constructed using :func:`vec()` and its variants.

.. data:: Pattern

  :status: Partially implemented

  A configuration of cells. Patterns with different shapes are different types.

.. _filter-types:

Set/filter types
================

.. data:: IntSet

  :status: Implementation in progress
  :operators: :ref:`set-operators`

  A finite set of :data:`Int`. Different :data:`IntSet` values are different types, and therefore cannot be stored in the same variable. (See :ref:`set-contents-rationale`)

  An :data:`IntSet` literal consists of a comma-separated list of :data:`Int` or :data:`IntSet` surrounded by curly braces. Examples:

  - ``{}`` constructs the empty set, containing no integers
  - ``{42}`` constructs a set containing only the integer 42
  - ``{1, 2, 3, 4}`` constructs a set containing the integers 1, 2, 3, and 4
  - ``{1, 2, 3, 4,}`` is also allowed (but discouraged unless spanning multiple lines)

  An :data:`IntSet` can also be constructed using a range literal consisting of two integers separated by ``..``. Examples:

  - ``1..5`` is equivalent to ``{1, 2, 3, 4, 5}``
  - ``-3..+3`` contains all integers from -3 to 3 (inclusive)
  - ``{-4..-1, 1..99}`` contains all integers from -4 to 99 (inclusive) *except* 0

.. data:: VecSet

  :status: Implementation in progress
  :operators: :ref:`set-operators`

  A finite set of :data:`Vec`, all with the same length. Different :data:`VecSet` values are different types, and therefore cannot be stored in the same variable. (See :ref:`set-contents-rationale`)

.. data:: CellSet

  :status: Partially implemented

  A set of cell states. Unlike :data:`IntSet` and :data:`VecSet`, all :data:`CellSet` values are the same type.

  This type's design is still a work in progress.

.. data:: PatternFilter

  :status: Not yet implemented

  Different :data:`PatternFilter` values are different types, and therefore cannot be stored in the same variable. (See :ref:`set-contents-rationale`)

  This type's design is still a work in progress.
