.. _totalistic-examples:

**********************
Totalistic CA examples
**********************

Conway's Game of Life
=====================

.. code-block::
  :linenos:

  @name "Conway's Game of Life"
  @author "HactarCE"
  @designer "John Conway"
  @year "1970"
  @url "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life"

  @pattern "bo$2bo$3o!"

  @transition {
    match nbhd.outer.count(#live) {
      case 3  { become #live }
      case 2  { remain       }
      default { become #dead }
    }
  }

Wireworld
=========

.. code-block::
  :linenos:

  @name "Wireworld"
  @author "HactarCE"
  @designer "Brian Silverman"
  @year "1987"
  @url "https://en.wikipedia.org/wiki/Wireworld"

  @pattern ".BA$C2.4C$.2C!"

  @states
    + _
    + #head
    + #tail
    + #wire

  @transition {
    match this {
      case #head { become #tail }
      case #tail { become #wire }
      case #wire {
        if nbhd.outer.count(#head) is 1..2 {
          become #head
        }
      }
    }
  }
