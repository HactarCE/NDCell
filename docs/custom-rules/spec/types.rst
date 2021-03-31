.. _types:

*****
Types
*****

NDCA uses a mix of `static`__ and `dynamic typing`__. During initialization, such as in ``@init`` blocks, types are dynamically checked. Types in compiled code, such as the transition function, are statically checked, and values of some types must be constant. (See :ref:`variability`.)

__ https://en.wikipedia.org/wiki/Type_system#Static_type_checking
__ https://en.wikipedia.org/wiki/Type_system#Dynamic_type_checking_and_runtime_type_information

Overview
========

- Primitive types:

  - :data:`Integer` --- 64-bit signed integer, also used for boolean values
  - :data:`Cell` --- cell state represented by an 8-bit unsigned integer
  - :data:`Tag` --- property held by some cell states
  - :data:`String` --- sequence of `Unicode`__ characters
  - :data:`Type` --- type value
  - :data:`Null` --- no value

- Collection types:

  - :data:`Vector` --- sequence of :data:`Integer` values
  - :data:`Array` --- masked N-dimensional array of :data:`Cell` values

- Predicate types:

  - :data:`IntegerSet` --- set of :data:`Integer` values
  - :data:`CellSet` --- set of :data:`Cell` values
  - :data:`VectorSet` --- set of :data:`Vector` values with the same length
  - :data:`Pattern` --- predicate that accepts or rejects :data:`Array` values of a certain size and shape
  - :data:`Regex` --- `regular expression`__

__ https://en.wikipedia.org/wiki/Unicode
__ https://en.wikipedia.org/wiki/Regular_expression

.. _dependent-types:

Dependent types
---------------

Some types are incomplete without an additional value. This value is written inside square brackets following the type name. The following are `dependent types`__:

- :data:`Vector` depends on its length (:data:`Integer`)

  - For example, ``Vector[3]`` is the type of a :data:`Vector` with length 3

- :data:`VectorSet` depends on the length of the vectors it contains (:data:`Integer`)

  - For example, ``VectorSet[4]`` is type of a :data:`VectorSet` that contains vectors of length 4

- :data:`Array` depends on its shape (:data:`VectorSet`)

  - For example, ``Array[[-1, -1]..[+1, +1]]`` is the type of an :data:`Array` that is 2D and contains the 9 cells in the rectangle from ``[-1, -1]`` to ``[+1, +1]``.

- :data:`Pattern` depends on the number of cells in the array that it matches (:data:`Integer`)

  - For example, ``Pattern[9]`` is the type of a :data:`Pattern` that matches arrays with 9 cells, arranged in any shape

__ https://en.wikipedia.org/wiki/Dependent_type

.. _variability:

Variability
-----------

In compiled code, such as the transition function, only :data:`Integer`, :data:`Cell`, :data:`Vector`, :data:`Array`, and :data:`CellSet` values may vary. All other values must be constant once the rule has been loaded.

The length of a :data:`Vector` value must be constant in compiled code. The size and shape of an :data:`Array` value also must be constant in compiled code.

None of these restrictions apply during initialization, such as in ``@init`` blocks.

Subtyping
---------

- :data:`Cell` and :data:`Tag` are subtypes of :data:`CellSet`
- :data:`Array` is a subtype of :data:`Pattern`

See :ref:`subtype-coercion` for more about subtyping.

.. _primitive-types:

Primitive types
===============

.. data:: Integer

  :status: Not yet implemented
  :methods: :ref:`integer-methods`
  :operators: :ref:`arithmetic-operators`, :ref:`bitwise-operators`, :ref:`comparison-operators`

  An integer, represented using a 64-bit signed `two's complement`__ integer. This means the minimum value is ``-9223372036854775808`` and the maximum value is ``9223372036854775807``.

  __ https://en.wikipedia.org/wiki/Two%27s_complement

  Boolean values are represented using integers. (See :ref:`boolean-conversion`.)

  An integer literal consists of a sequence of digits, optionally prefixed by a unary ``+`` or ``-`` operator. Underscores may be present after the first digit, and do not affect the value. Examples:

    - ``0``
    - ``-1``
    - ``42``
    - ``+6``
    - ``-32_768``

  If an integer literal is prefixed by ``0b``, ``0x``, or ``0o``, it will be parsed as a `binary`__, `hexadecimal`__ and `octal`__ literal respectively. Examples:

  __ https://simple.wikipedia.org/wiki/Binary
  __ https://simple.wikipedia.org/wiki/Hexadecimal
  __ https://simple.wikipedia.org/wiki/Octal

    - ``0xA6f1`` (hexadecimal literal equivalent to ``42737``)
    - ``0b_1110_1101_1110_1110`` (binary literal equivalent to ``60910``)
    - ``-0o777`` (octal literal equivalent to ``-511``)

.. data:: Cell

  :status: Not yet implemented
  :methods: :ref:`cell-methods`
  :operators: :ref:`set-operators`, :ref:`comparison-operators` (``==`` and ``!=`` only)
  :supertype: :data:`CellSet`

  A cell state, represented using an 8-bit unsigned integer. This means the minimum value is ``0`` and the maximum value is ``255``, so an automaton cannot have more than 256 states. :data:`Cell` values are always within the range of valid cell states in a cellular automaton. For example, an automaton with 10 states has a maximum cell state ID of ``9``.

  :data:`Cell` is a subtype of :data:`CellSet`. When used in place of a :data:`CellSet`, a :data:`Cell` represents a set containing only the one cell state.

  A :data:`Cell` literal consists of the ``#`` operator followed by the cell state ID. Examples:

    - ``#0``
    - ``#1``
    - ``#42``

  A :data:`Cell` literal may use an arbitrary integer expression for the cell state ID by surrounding the expression in parentheses. Examples:

    - ``#(my_integer_variable)``
    - ``#(x + 5)``

.. data:: Tag

  :status: Not yet implemented

  This type's design is still a work in progress.

.. data:: String

  :status: Partially implemented

  This type's design is still a work in progress.

.. _collection-types:

Collection types
================

.. data:: Vector

  :status: Not yet implemented
  :methods: :ref:`vector-methods`
  :operators: :ref:`arithmetic-operators`, :ref:`bitwise-operators`, :ref:`comparison-operators`, :ref:`vector-indexing`

  A vector, represented using a fixed-length array of :data:`Integer` values. Each :data:`Integer` value is a component of the :data:`Vector`, and the number of components is the length of the :data:`Vector`. The length of a :data:`Vector` must be between 1 and 256 (inclusive). The type of a :data:`Vector` depends on its length. (See :ref:`dependent-types`.)

  The first component of a :data:`Vector` is the X component at index 0; the second is the Y component at index 1; etc.

  A :data:`Vector` literal consists of a list of integer expressions separated by commas surrounded by square brackets. Examples:

  - ``[3, -1, 0]`` (:data:`Vector` of length ``3`` with X component ``3``, Y component ``-1``, Z component ``0``)
  - ``[6]`` (:data:`Vector` of length ``1`` with X component ``6``)
  - ``[a, b]`` (:data:`Vector` of length ``2`` with X compoment ``a`` and Y component ``b``, given ``a`` and ``b`` are variables containing integers)

  A :data:`Vector` literal may contain other vectors, which are concatenated to produce the result. Examples:

  - ``[v1, -3, v2]`` (:data:`Vector` constructed by concatenating ``v1``, ``[-3]``, and ``v2``)

  A :data:`Vector` can also be constructed using :func:`vec()` and its variants.

.. data:: Pattern

  :status: Partially implemented

  A configuration of cells. Patterns with different shapes are different types.

.. _filter-types:

Set/filter types
================

.. data:: IntegerSet

  :status: Implementation in progress
  :operators: :ref:`set-operators`

  A finite set of :data:`Integer`.

  An :data:`IntegerSet` literal consists of a comma-separated list of :data:`Integer` or :data:`IntegerSet` surrounded by curly braces. Examples:

  - ``{}`` constructs the empty set, containing no integers
  - ``{42}`` constructs a set containing only the integer 42
  - ``{1, 2, 3, 4}`` constructs a set containing the integers 1, 2, 3, and 4
  - ``{1, 2, 3, 4,}`` is also allowed (but discouraged unless spanning multiple lines)

  An :data:`IntegerSet` can also be constructed using a range literal consisting of two integers separated by ``..``. Examples:

  - ``1..5`` is equivalent to ``{1, 2, 3, 4, 5}``
  - ``-3..+3`` contains all integers from -3 to 3 (inclusive)
  - ``{-4..-1, 1..99}`` contains all integers from -4 to 99 (inclusive) *except* 0

.. data:: CellSet

  :status: Partially implemented

  This type's design is still a work in progress.

.. data:: VectorSet

  :status: Implementation in progress
  :operators: :ref:`set-operators`

  A finite set of :data:`Vector`, all with the same length.

.. data:: Pattern

  :status: Not yet implemented

  This type's design is still a work in progress.

.. data:: Regex

  :status: Not yet implemented

  This type's design is still a work in progress.
