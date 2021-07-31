.. _variables:

*********
Variables
*********

.. note::

  This page is under construction.

.. _variable-types:

Variable typing
===============

NDCA is `statically typed`__, which means that each variable has one :ref:`type <types>` of value it can store. Variable types are automatically inferred based on the values assigned to them.

__ https://en.wikipedia.org/wiki/Type_system#Static_type_checking

.. _variable-names:

Variable names
==============

TODO: list reserved words here

.. _variable-assignment:

Variable assignment
===================

Semantics
=========

Variables use `value semantics`__, which means that modifying a value in one variable does not have an effect on any other variables.

__ https://en.wikipedia.org/wiki/Value_semantics

Built-in variables
==================

These variables are automatically available in every program.

.. data:: this

  The state of the current cell being updated. This variable is immutable.

  :type: :data:`Cell`

.. data:: nbhd

  Alias for :data:`neighborhood`.

  :type: :data:`CellArray`

.. data:: neighborhood

  The pattern of cells surrounding the current cell. This variable is immutable.

  :type: :data:`CellArray`
