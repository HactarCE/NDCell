.. _formats:

************
File formats
************

N-dimensional Extended RLE format (NDRLE)
=========================================

For interchanging small patterns, NDCell uses a run-length encoded format that is backwards-compatible with Golly's `Extended RLE format`__. It introduces the following new features:

__ http://golly.sourceforge.net/Help/formats.html#rle

- The number of dimensions is inferred from the rule.
- The header containing the size of the pattern along the X and Y axes may also contain Z, W, U, and V coordinates as necessary. For example: ``x = 6, y = 3, z = 4, w = 2, rule = SomeRule4D``. (This is backwards-compatible; Golly ignores unknown parameters.)
- The ``#CXRLE`` line may contain more or fewer comma-separated integers for ``Pos``, depending on the number of dimensions. Missing values are assumed to be ``0``, and extra values are ignored. (This is backwards-compatible; Golly uses the same behavior, but always assumes 2D.)
- The ``#CXRLE`` line may contain a negative value for ``Gen``. (This is backwards-compatible; Golly ignores invalid ``Gen`` values, using ``0`` instead.)
- The following special symbols are used to delimit rows/layers/etc:

  - ``$`` to terminate a 1D row, advancing along the Y axis (as in normal RLE)
  - ``/`` to terminate a 2D layer, advancing along the Z axis
  - ``%W`` to terminate a 3D layer, advancing along the W axis
  - ``%U`` to terminate a 4D layer, advancing along the U axis
  - ``%V`` to terminate a 5D layer, advancing along the V axis
  - ``!`` to terminate a pattern (as in normal RLE)

Conventionally Y coordinates increase downwards in an RLE; in NDCell, however, Y coordinates increase upwards. To resolve this, NDCell treats all coordinates in the ``#CXRLE`` ``Pos`` values as negated except for the X value. (This is consistent with Golly's behavior when the setting "Y coordinates increase upwards" is enabled.)
