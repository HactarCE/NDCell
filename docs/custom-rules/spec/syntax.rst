******
Syntax
******

.. note::

  This page is under construction.

Like Lua and most C-family programming languages:

- :ref:`Code blocks <code-block>` begin with ``{`` and end with ``}``
- Line comments begin with ``//`` and end with a newline
- Block comments begin with ``/*`` and end with ``*/`` (and cannot be nested)
- All whitespace is equivalent, used to separate tokens, with the following exceptions:

  - Strings, where whitespace is interpreted literally
  - Line comments, where only a newline ends a line comment

Unless you really care about formalism, you're better off reading the :ref:`examples <examples>` to get a jist of the syntax.

.. _token-syntax:

Tokens
======

The file is split into tokens, where each token is one of the following:

- Line comment beginning with ``//`` and ending with the next newline
- Block comment beginning with ``/*`` and ending with the next ``*/``
- String beginning and ending with ``"``  (may contain any character except ``"``)
- String beginning and ending with ``'`` (may contain any character except ``'``)
- Number with a decimal point, matching the `regex`_ ``-?\d?\.\d+`` (currently unused)
- Number without a decimal point, matching the `regex`_ ``-?\d+``
- Keyword (see :ref:`keywords`)
- Identifier, matching the `regex`_ ``[A-Za-z_][A-Za-z_\d]*``
- Tag name, consisting of ``#`` followed immediately by an identifier (no space)
- Directive name, consisting of ``@`` followed immediately by an identifier (no space)
- One of the following literal character sequences:

  - Assignment operator `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `**=`, `<<=`, `>>=`, or `>>>=`
  - Operator `..`, `**`, `<<`, `>>`, or `>>>`
  - Relational operator `==`, `!=`, `<=`, or `>=`

- Any other single Unicode character from the letter, numeral, punctuation, or symbol categories. (See `Unicode Character Categories`__.)

__ https://www.compart.com/en/unicode/category

.. _regex: https://en.wikipedia.org/wiki/Regular_expression

Note that newlines are allowed in strings.

NOTE: string syntax may change in the future

NOTE: either document and use string prefix characters, or remove support for them from the lexer

.. _keywords:

Keywords
========

The following keywords are reserved, and cannot be used for identifiers:

- ``and``
- ``assert``
- ``become``
- ``bind``
- ``bound``
- ``break``
- ``case``
- ``colors``
- ``continue``
- ``else``
- ``error``
- ``for``
- ``icons``
- ``if``
- ``in``
- ``is``
- ``match``
- ``models``
- ``not``
- ``or``
- ``remain``
- ``return``
- ``same``
- ``static``
- ``transition``
- ``unless``
- ``where``
- ``while``
- ``with``
- ``xor``

Some of these are currently used, and some are reserved for future use.

.. _file-syntax:

File structure
==============

An NDCA file consists of a sequence of :ref:`directives <directive-syntax>`.

.. _directive-syntax:

Directives
==========

Directives begin with a directive name, which always begins with ``@``, followed by an :ref:`expression <expressions>`, :ref:`code block <code-block>`, or other construct depending on the specific directive. See :ref:`directives` for more.

.. code-block::

  @directive_name directive_value

.. _code-block:

Code blocks
===========

A code block begins with a single ``{``, contains zero or more :ref:`statements <statements>`, and ends with a single ``}``. Statements are *not* separated by a semicolon. Although it is conventional for each statement to be placed on its own line, this is optional; newlines can be inserted anywhere or omitted completely.

.. code-block::

  {
    first statement
    second statement
  }
