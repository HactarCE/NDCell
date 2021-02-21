.. _directives:

**********
Directives
**********

All directives are optional.

Automaton directives
====================

.. data:: @states

  The number of states in the automaton.

  :status: Partially implemented; more functionality planned
  :type: :data:`Int`
  :default value: ``2``
  :examples:

    - ``@states 4``
    - ``@states 256``

.. data:: @radius

  The maximum extent of the neighborhood used by the transition function, measured using `Chebyshev distance`__.

  __ https://en.wikipedia.org/wiki/Chebyshev_distance

  :status: Fully implemented
  :type: :data:`Int` (nonnegative)
  :default value: ``1``
  :examples:

    - ``@radius 2``

.. data:: @transition

  The transition function for the automaton.

  TODO: describe special variables ``this``, ``neighborhood``, and ``nbhd``.

  :status: Fully implemented
  :type: :ref:`Code block <code-block>`
  :default value: ``{ remain }``
  :examples: See :ref:`examples`

.. data:: @pattern

  A default pattern for the automaton, specified as `RLE`__.

  __ https://www.conwaylife.com/wiki/Run_Length_Encoded

  :status: Not yet implemented; may be removed
  :type: :data:`String`
  :default value: None
  :examples:

    - ``@pattern "bo$2bo$3o!"`` (a glider)

Metadata directives
===================

.. data:: @name

  The name of the automaton.

  :status: Not yet implemented
  :type: :data:`String`
  :default value: Name of the file, excluding the extension.
  :examples:

    - ``@name "Conway's Game of Life"``
    - ``@name "Wireworld"``

.. data:: @author

  The author(s) of the NDCA file.

  :status: Not yet implemented
  :type: :data:`String`
  :default value: ``"Unknown"``
  :examples:

    - ``@author "HactarCE"``
    - ``@author "John Smith"``
    - ``@author "HactarCE and John Smith"``

.. data:: @designer

  The designer(s)/discoverer(s) of the automaton.

  :status: Not yet implemented
  :type: :data:`String`
  :default value: Same as :data:`@author`
  :examples:

    - ``@designer "John Conway"``
    - ``@designer "Daniel B. Miller and Edward Fredkin"``
    - ``@designer "Steven Wolfram"``

.. data:: @year

  The year that the automaton was designed/discovered.

  :status: Not yet implemented
  :type: :data:`String`
  :default value: ``"Unknown"``
  :examples:

    - ``@year "2010"``
    - ``@year "circa 1970"``

.. data:: @url

  A link with more information about the automaton, generally the research paper or website where the automaton was first published, or a Wikipedia or `LifeWiki`__ page.

  __ https://www.conwaylife.com/wiki/Main_Page

  :status: Not yet implemented
  :type: :data:`String`
  :default value: ``"None"``
  :examples:

    - ``@url "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"``
    - ``@url "http://busyboxes.org/faq.html"``
    - ``@url "https://www.conwaylife.com/wiki/OCA:Star_Wars"``
