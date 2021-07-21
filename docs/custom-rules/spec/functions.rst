*********
Functions
*********

.. function:: bool(value)

  :status: Fully implemented
  :param value: The value to convert to a boolean
  :type value: any basic type
  :rtype: :data:`TRUE` if the value is truthy, or :data:`FALSE` if it is falsey

  Converts a value to a boolean. See :ref:`boolean-conversion`.



.. function:: vec([value=0], [len=NDIM])

  :status: Fully implemented
  :param value: The value to convert to a vector
  :type value: integer or vector
  :param len: The length of the resulting vector
  :type len: keyword argument; constant integer between 1 and 256 (inclusive)
  :rtype: vector

  Converts a value to a vector of length :data:`len`. See :ref:`vector-vector-conversion` and :ref:`integer-vector-conversion`.

  Examples without ``len`` argument:

  - ``vec()`` returns ``[0, 0, 0]`` when ``NDIM == 3``
  - ``vec(-6)`` returns ``[-6, -6]`` when ``NDIM == 2``
  - ``vec([-6])`` returns ``[-6, 0]`` when ``NDIM == 2``
  - ``vec([1, 2, 3, 4])`` returns ``[1, 2]`` when ``NDIM == 2``

  Examples with ``len`` argument:

  - ``vec(len=5)`` returns ``[0, 0, 0, 0, 0]``
  - ``vec(10, len=2)`` returns ``[10, 10]``
  - ``vec([-1, -2, -3, -4], len=1)`` returns ``[-1]``
  - ``vec(len=NDIM+1)`` returns ``[0, 0, 0, 0]`` when ``NDIM == 3``

  This function has a variant for each possible vector length from 1 to 256: :func:`vec1()`, :func:`vec2()`, :func:`vec3()`, etc. up to :func:`vec256()`. For example, :func:`vec3()` converts its argument to a vector of length 3. These variants do not accept the ``len`` keyword argument.

  Examples:

  - ``vec5()`` returns ``[0, 0, 0, 0, 0]``
  - ``vec2(10)`` returns ``[10, 10]``
  - ``vec1([-1, -2, -3, -4])`` returns ``[-1]``
