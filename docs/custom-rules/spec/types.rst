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

  - For example, ``Array[[-1, -1]..[1, 1]]`` is the type of an :data:`Array` that is 2D and contains the 9 cells in the rectangle from ``[-1, -1]`` to ``[1, 1]``.

- :data:`Pattern` depends on the number of cells in the array that it matches (:data:`Integer`)

  - For example, ``Pattern[9]`` is the type of a :data:`Pattern` that matches arrays with 9 cells, arranged in any shape

__ https://en.wikipedia.org/wiki/Dependent_type

.. _variability:

Variability
-----------

In compiled code, such as the transition function, only :data:`Integer`, :data:`Cell`, :data:`Tag`, :data:`Vector`, :data:`Array`, and :data:`CellSet` values may vary. All other values must be constant in compiled code.

The length of a :data:`Vector` value must be constant in compiled code. The size and shape of an :data:`Array` value also must be constant in compiled code.

None of these restrictions apply during initialization, such as in ``@init`` blocks.

Subtyping
---------

- :data:`Cell` and :data:`Tag` are subtypes of :data:`CellSet`
- :data:`Array` is a subtype of :data:`Pattern`
- :data:`EmptySet` is a subtype of :data:`IntegerSet`, :data:`CellSet`, and :data:`VectorSet`

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

  :data:`Cell` is a subtype of :data:`CellSet`. When used in place of a :data:`CellSet`, a :data:`Cell` coerces to a set containing only the one cell state. See :ref:`subtype-coercion` for more about subtyping.

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

  - ``[3, -1, 0]`` has length ``3``, X component ``3``, Y component ``-1``, and Z component ``0``
  - ``[6]`` has length ``1`` and X component ``6``
  - ``[a, b]`` has length ``2``, X compoment ``a`` and Y component ``b``, given ``a`` and ``b`` are variables containing integers

  A :data:`Vector` literal may contain other vectors, which are concatenated to produce the result. Examples:

  - Suppose ``v1 = [1, 2]`` and ``v2 = [7]``; then ``[v1, -3, v2]`` is equivalent is ``[1, 2, -3, 7]``

  A :data:`Vector` can also be constructed using :func:`vec()` and its variants.

.. data:: Array

  :status: Partially implemented

  A configuration of cells. Arrays with different shapes are different types.

.. _filter-types:

Set/filter types
================

.. data:: EmptySet

  :status: Fully implemented
  :operators: :ref:`set-operators`
  :supertypes: :data:`IntegerSet`, :data:`CellSet`, :data:`VectorSet`

  The empty set of any type. The empty set literal is ``{}``.

  :data:`EmptySet` is a subtype of :data:`IntegerSet`, :data:`CellSet`, and :data:`VectorSet`. When used in place of a :data:`IntegerSet`, :data:`CellSet`, or :data:`VectorSet`, a :data:`EmptySet` coerces to an empty set of that of that type. See :ref:`subtype-coercion` for more about subtyping.

.. data:: IntegerSet

  :status: Not yet implemented
  :operators: :ref:`set-operators`

  A finite set of :data:`Integer`.

  An :data:`IntegerSet` literal consists of a comma-separated list of :data:`Integer` and :data:`IntegerSet` values surrounded by curly braces. Examples:

  - ``{42}`` is a set containing only the integer 42
  - ``{1, 2, 3, 4}`` is a set containing the integers 1, 2, 3, and 4
  - ``{1, 2, 3, 4,}`` is also allowed (but discouraged unless spanning multiple lines)

  An empty set literal (``{}``) has type ``EmptySet`` rather than ``IntegerSet``, so the empty :data:`IntegerSet` is written ``IntegerSet.empty``. This is usually unnecessary, though, because ``EmptySet`` coerces to an ``IntegerSet`` when used where one would be expected. (See :ref:`subtype-coercion`.)

  An :data:`IntegerSet` can also be constructed using a range literal consisting of two integers separated by ``..``. Examples:

  - ``1..5`` is equivalent to ``{1, 2, 3, 4, 5}``
  - ``-3..+3`` is a set containing all integers from -3 to 3 (inclusive)
  - ``{-1, 1..99, 101..120}`` is a set containing all integers from -1 to 120 (inclusive) *except* 0 and 100

.. data:: CellSet

  :status: Not yet implemented

  This type's design is still a work in progress.

  An empty set literal (``{}``) has type ``EmptySet`` rather than ``CellSet``, so the empty :data:`CellSet` is written ``CellSet.empty``. This is usually unnecessary, though, because ``EmptySet`` coerces to an ``CellSet`` when used where one would be expected. (See :ref:`subtype-coercion`.)

.. data:: VectorSet

  :status: Partially implemented
  :operators: :ref:`set-operators`

  A finite set of :data:`Vector`, all with the same length.

  A :data:`VectorSet` literal consists of a comma-separated list of :data:`Vector`, :data:`VectorSet`, and :data:`Integer` values surrounded by curly braces. At least one value must be a :data:`Vector` or :data:`VectorSet`; otherwise it is an :data:`IntegerSet` literal. All values are converted to the longest vector in the set. (See :ref:`vector-vector-conversion` and :ref:`integer-vector-conversion`.) Examples:

  - ``{[1, 2], [3, 4]}`` is a set containing the vectors ``[1, 2]`` and ``[3, 4]``
  - ``{[18], 12, [9, -4, 6]}`` is a set containing

  An empty set literal (``{}``) has type ``EmptySet`` rather than ``VectorSet``, so the empty :data:`VectorSet` for a vector length ``l`` is written ``VectorSet[l].empty``. This is usually unnecessary, though, because ``EmptySet`` coerces to an ``VectorSet`` when used where one would be expected. (See :ref:`subtype-coercion`.) Examples:

  - ``VectorSet[3].empty`` is the empty set of vectors with length 3.
  - ``VectorSet[NDIM].empty`` is the empty set of vectors with length :data:`NDIM`.

  A :data:`VectorSet` can also be constructed using a range literal consisting of two vectors separated by ``..``, or a vector and an integer (in any order) separated by ``..``. Examples:

  - ``[1, 2]..[3, 4]`` is a set containing all vectors in the rectangle from ``[1, 2]`` to ``[3, 4]`` (inclusive)
  - ``{}``

  The volume of the bounding rectangle of a :data:`VectorSet` cannot exceed 65536.

.. data:: Pattern

  :status: Not yet implemented

  This type's design is still a work in progress.

.. data:: Regex

  :status: Not yet implemented

  This type's design is still a work in progress.
