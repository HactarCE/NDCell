.. _directional-examples:

********************
Directional examples
********************

Directional wires
=================

NOTE: this rule uses several features that are not yet implemented and VERY subject to change.

.. code-block::

  @name "Directional wires demo"
  @author "HactarCE"

  @states
    + _
    + #wire
      // Wire points in one of four directions
      * (#e + #n + #w + #s)
      * (_ + #excited)
    + #splitter
      // Splitter points in all directions at once
      * #e * #n * #w * #s
      * (_ + #excited)

  // Custom cell state rotations
  @rot90_xy #e -> #n -> #w -> #s

  @transition {
    if this is #wire | #splitter {
      excite = (nbhd[E] is #excited & #w and this is ~#e | #splitter)
            or (nbhd[N] is #excited & #s and this is ~#n | #splitter)
            or (nbhd[W] is #excited & #e and this is ~#w | #splitter)
            or (nbhd[S] is #excited & #n and this is ~#s | #splitter)
      become (#wire, #splitter, #e, #n, #w, #s) from this
            & #excited:(excite)
    }
  }

HPP model
=========

NOTE: this rule uses several features that are not yet implemented and VERY subject to change.

.. code-block::

  @name "HPP"
  @author "HactarCE"
  @designer "Hardy, Pomeau, and de Pazzis"
  @year "1973"
  @url "https://en.wikipedia.org/wiki/HPP_model"

  // State IDs are compatible with Golly's HPP implementation
  @states
    + (#space + #wall)
      * (_ + #w)
      * (_ + #n)
      * (_ + #e)
      * (_ + #s)
    + #sink
    + #source * #w * #n * #e * #s

  // Custom cell state rotations
  @rot90_xy #e -> #n -> #w -> #s

  @bindings `
    where < is #w
    where ^ is #n
    where > is #e
    where v is #s
    where . is ~(#n | #e | #s | #w)
  `

  @mask `
    . # .
    # # #
    . # .
  `

  @transition {
    this_type = (#space, #wall) from this
    with symmetry rotate4 {
      match nbhd {
        // No particles
        case `
          .
        . * .
          .
        ` { become this_type }
        // One particle
        case `
            .
          > * .
            .
        ` { become this_type & #e }

        // Two particles
        case `
            v
          > * .
            .
        ` { become this_type & #e & #s }
        case `
            .
          > * <
            .
        ` { become this_type & #n & #s }

        // Three particles
        case `
            v
          > * <
            .
        ` { become this_type & #e & #n & #w }

        // Four particles
        case `
            v
          > * <
            ^
        ` { become this_type & #e & #n & #w & #s }
      }
    }
  }
