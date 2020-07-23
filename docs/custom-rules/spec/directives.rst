**********
Directives
**********

All directives are optional.

.. data:: @name

  The name of the automaton.

  NOTE: not yet implemented

  :type: :data:`String`
  :default value: Name of the file, excluding the extension.
  :examples:

    - ``@name "Conway's Game of Life"``
    - ``@name "Wireworld"``

.. data:: @states

  The number of states in the automaton.

  NOTE: ``@states`` will be made much more powerful in the future.

  :type: :data:`Integer`
  :default value: ``2``
  :examples:

    - ``@states 4``
    - ``@states 256``

.. data:: @transition

  The transitioin function for the automaton.

  TODO: describe special variables ``this``, ``neighborhood``, and ``nbhd``.

  :type: Code block
  :default value: ``{ remain }``
  :examples: See :doc:`examples`
