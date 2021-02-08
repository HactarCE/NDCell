*********
Functions
*********

.. function:: bool(value)

  :param value: The value to convert to a boolean
  :type value: any basic type
  :rtype: :data:`TRUE` if the value is truthy, or :data:`FALSE` if it is falsey

  Converts a value to a boolean. See :ref:`boolean-conversion`.

.. function:: vec([value=0])

  :param value: The value to convert to a vector
  :type value: integer or vector
  :rtype: vector

  Converts a value to a vector of length :data:`NDIM`. See :ref:`vector-vector-conversion` and :ref:`integer-vector-conversion`.

  This function has a variant for each possible vector length from 1 to 256: :func:`vec1()`, :func:`vec2()`, :func:`vec3()`, etc. up to :func:`vec256()`.
