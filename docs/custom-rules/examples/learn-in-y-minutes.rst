.. _learn-in-y-minutes:

***********************
Learn NDCA in Y minutes
***********************

TODO: Remove this in favor of multiple smaller examples.

Here is a `Learn X in Y Minutes`__-style runtime of NDCA:

__ https://learnxinyminutes.com/

.. code-block::
  :linenos:

  // C-style comments
  /* All whitespace is equivalent (like Lua),
     so newlines and indentation are arbitrary. */

  // Name of the cellular automaton (inferred from filename if not specified)
  @name "Learn In Y Minutes"

  // Number of states (default is 2; max is 256)
  @states 5

  // Transition function (default does nothing)
  @transition {

    // Integers are 64-bit signed.
    x = 3

    // Conditional statement:
    if x == 3 {
      // `#` turns a number into a cell state.
      cell1 = #1
    } else {
      // Arguments to `#` other than a literal number must be surrounded in
      // parentheses.
      cell2 = #(x)

      // Abort the simulation, with an optional message.
      error "Oh no! x isn't 3"
    }

    // Math
    x = x * 10
    x -= 7

    // Assertions
    assert x == 23
    // ... with an optional message.
    assert x > 0, "Why is x negative?"

    // Even though cell2 hasn't been set, it's been defined as a variable
    // containing a cell. All variables have a default value (generally 0).
    assert cell2 == #0

    // Variables are statically typed, but all types are inferred.
    // This would cause an error:
    /* x = #2 */

    // The variables `this` and `neighborhood` (and its alias `nbhd`)
    // are automatically defined inside the transition function. `this`
    // is a cell state.
    if this == cell1 {
      // Return from the transition function and stay the same state.
      remain
    } else if this == cell2 {
      // Return from the transition function and change to state #2.
      become #2
    }

  }
