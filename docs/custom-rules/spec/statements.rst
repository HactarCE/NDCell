.. _statements:

**********
Statements
**********

Several statements involve boolean conversion; for more on that, see :ref:`boolean-conversion`.

.. contents::
   :local:
   :backlinks: entry

Code block
==========

A :ref:`code block <code-block>` is a valid statement.

.. _var-assign-statement:

Variable assignment
===================

A variable assignment consists of a left-hand side (LHS) :ref:`expression <expressions>` (which must be :ref:`assignable <assignable-expressions>`), followed by an assignment operator, and then a right-hand side (RHS) :ref:`expression <expressions>` to be evaluated and assigned to the LHS.

The assignment operator must be either ``=``, ``+=``, ``-=``, ``*=``, ``/=``, ``%=``, ``&=``, ``|=``, ``^=``, ``**=``, ``<<=``, ``>>=``, or ``>>>=``. ``=`` assigns the value of the RHS to the LHS directly and discards the previous value of the LHS expression. All the other operators first evaluate the LHS and then perform some operation using the values of the LHS and RHS expressions. For example, ``X += Y`` (where ``X`` and ``Y`` are any arbitrary expressions) is equivalent to ``X = X + Y``.

Examples:

.. code-block::

  i = 0
  my_variable = #(1 << 3)
  my_vector[i] *= -8 * 2
  my_vector.y /= 6

.. _branching-statements:

Branching statements
====================

.. _if-statement:

If statement
------------

An if statement consists of the keyword ``if``, followed by a condition :ref:`expression <expressions>`, and then a :ref:`code block <code-block>`. The condition expression must be able to be converted to a boolean. An if statement evaluates the condition expression and if the result is truthy it executes the statements inside the code block.

Example:

.. code-block::

  if value_a {
    // value_a is truthy
  }

.. _else-if-statement:

Else-if statement
-----------------

An else-if statement consists of the keyword ``else``, followed by an :ref:`if statement <if-statement>`. The condition expression must be able to be converted to a boolean. An else-if statement must immediately follow an if statement or another else-if statement. If all of the condition expressions of the immediately preceding if statements and else-if statements were falsey, the else-if statement will execute evaluate its own expression, and if it is truthy it executes the statements inside the code block.

Examples:

.. code-block::

  if value_a {
    // value_a is truthy
  } else if value_b {
    // value_a is falsey
    // but value_b is truthy
  } else if value_c {
    // value_a and value_b are both falsey
    // but value_c is truthy
  }

.. _else-statement:

Else statement
--------------

An else statement consists of the keyword ``else``, followed by a :ref:`code block <code-block>`. An else statement must immediately follow an if statement or else-if statement. If all of the condition expressions of the immediately preceding if statements and else-if statements were falsey, the else statement will the statements inside the code block.

Examples:

.. code-block::

  if value_a {
    // value_a is truthy
  } else {
    // value_a is falsey
  }

  if value_a {
    // value_a is truthy
  } else if value_b {
    // value_a is falsey
    // but value_b is truthy
  } else {
    // value_a and value_b are both falsey
  }

.. _loops:

Loops
=====

.. _for-loop:

For loop
--------

An if statement consists of the keyword ``for``, followed by a left-hand side (LHS) :ref:`expression <expressions>` (which must be :ref:`assignable <assignable-expressions>`), the keyword ``in``, a right-hand side (RHS) :ref:`expression <expressions>`, and then a :ref:`code block <code-block>`. The RHS must evaluate to an iterable type (TODO: link this to something, or explain it right here) and the LHS must evaluate to the iteration type of the RHS (TODO: explain this better). The statements inside the code block are executed for each iteration value (TODO: explain what this means, or use a better term) of the RHS.

Example:

.. code-block::

  for i in 1..10 {
    // code here executes 10 times

    // on the first iteration, i is 1
    // on the second, i is 2
    // etc.
    // on the last iteration, i is 10
  }

.. _break-statement:

Break statement
---------------

A break statement consists of the keyword ``break``. It causes execution to jump to immediately after the innermost loop being executed. After a break statement, the loop will not iterate further.

Example:

.. code-block::

  for i in 1..10 {
    // code here executes only when i is 1, 2, or 3

    if i == 3 {
      break
    }

    // code here executes only when i is 1 or 2
  }

.. _continue-statement:

Continue statement
------------------

A continue statement consists of the keyword ``continue``. It causes execution to jump to the end of the current iteration of the innermost loop being executed. After a continue statement, the loop will continue iterating if the exit condition is not yet met.

Example:

.. code-block::

  for i in 1..10 {
    // code here executes for all numbers from 1 to 10

    if i % 3 != 1 {
      continue
    }

    // code here executes only when i is 1, 4, 7, or 10
  }

.. _debugging:

Debugging
=========

.. _error-statement:

Error statement
---------------

An error statement consists of the keyword ``error``, optionally followed by a :ref:`string literal <string-literal>` specifying a custom error message. An error statement causes an error, which aborts the simulation.

Examples:

.. code-block::

  error
  error "I've got a bad feeling about this"

.. _assert-statement:

Assert statement
----------------

An assert statement consists of the keyword ``assert``, followed by an :ref:`expression <expressions>`, and then an optional comma and :ref:`string literal <string-literal>` specifying a custom error message. The expression must be able to be converted to a boolean. An assert statement evaluates the expression and if the result is falsey it causes an error, which aborts the simulation.

Examples:

.. code-block::

  assert x > 0 // where x is an Integer variable
  assert 2 + 2 == 4, "math is broken!"

.. _returning-statements:

Returning statements
====================

.. _become-statement:

Become statement
----------------

A become statement consists of the keyword ``become``, followed by an :ref:`expression <expressions>`. The expression must evaluate to a :data:`Cell`. A become statement evaluates the expression and transitions the current cell to the value of that expression. Become statements can only be used in transition functions (see :data:`@transition`).

TODO: cell filter in become statement? also provide example

Example:

.. code-block::

  become #10

.. _remain-statement:

Remain statement
----------------

A remain statement consists of the keyword ``remain``. It is equivalent to ``become this``. NOTE: this may change in the future. what if ``this`` is modified?

Example:

.. code-block::

  remain

.. _return-statement:

Return statement
----------------

A return statement consists of the keyword ``return``, followed by an :ref:`expression <expressions>`. The expression must evaluate to a value of the same type as the return type of the function. A return statement evaluates the expression and transitions the current cell to the value of that expression. Return statements can only be used in helper functions (see :data:`@function`).

Example:

.. code-block::

  return 10 * x // where x is an Integer variable
