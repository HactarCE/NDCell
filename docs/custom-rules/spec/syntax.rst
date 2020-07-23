******
Syntax
******

.. _directives:

Directives
==========

.. code-block::

  @directive_name directive_value

.. _if-statement:

Conditional statements
======================

An ``if`` statement branches based on a boolean value, and can be followed by any number of ``else`` and ``else if`` clauses.

.. code-block::

  if value_a {
    // value_a is true
  }

  if value_a {
    // value_a is true
  } else {
    // value_a is false
  }

  if value_a {
    // value_a is true
  } else if value_b {
    // value_a is false but
    // value_b is true
  } else {
    // value_a is false and
    // value_b is also false
  }
