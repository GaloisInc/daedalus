Implicit Parameters
===================

An *implicit parameter* is a parameter that is automatically
passed along by the system, which helps avoid clutter in specifications.
In DaeDaLus, implicit parameters have names staring with ``?``, for example
``?bigendian``.

Implicit parameters are useful in situations where the value of a parameter
is set once for a given scope, and then the same parameter is just passed
along with no changes to the "leaves" of the specification.   This is quite
common in situations where some configuration options are read once, and then
are just passed along for the rest of a parser.

Here is an example of a function that uses an implicit parameter to concatenate
two bit vectors one way or another:

.. code-block:: DaeDaLus

  def joinWords a b =
    if ?bigendian     -- ?bigendian is an implicit parameter
      then a # b
      else b # a

Parsers automatically inherit the implicit parameters needed by functions
or parsers they use.  For example, here are two parsers that can be used
to parse either big-endian or little-endian words, depending on the value
of the implicit parameter ``?bigendian``:

.. note::
  These parsers use :ref:`Implicit Lifting` to make them more readable


.. code-block:: DaeDaLus

  def Word16 = joinWords UInt8 UInt8
  def Word32 = joinWords Word16 Word16

If a ``block`` provides a value for an implicit parameter, then all calls
for the rest of the block will use that value for the parameter.  For example,
``BEWord16`` *does not* have an implicit parameter:

.. code-block:: DaeDaLus

  def BEWord16 =
    block
      let ?bigendian = true
      Word16    -- `?bigendian` has the value `true`

It is possible to use different values for the same implicit parameter,
as illustrated by the following example:

.. code-block:: DaeDaLus

  def Example =
    block
      -- Just for testing, we set the input stream to a known value
      SetStream (arrayStream (concat [ [0,1,0,0,0,1]
                                     , [1,0,1,0,0,0] ]))

      big =
        -- Here we define the value of an implicit parameter
        -- in all uses for the rest of the block
        block
          let ?bigendian = true
          x = Word16
          y = Word32
      little =
        -- This block uses a different value for the implicit parameter
        block
          let ?bigendian = false
          x = Word16
          y = Word32

Executing ``Example`` results in the following output:

.. code-block:: bash

  { big: { x: 1[16]
         , y: 1[32]
         }
  , little: { x: 1[16]
            , y: 1[32]
            }
  }











