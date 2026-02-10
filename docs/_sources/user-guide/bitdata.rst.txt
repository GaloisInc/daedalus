Bitdata
=======

The ``bitdata`` construct provides a convenient way to break bytes into
groups of bits, which are then combined into a tagged union. 

.. code-block:: DaeDaLus

  bitdata ChooseOption where 
    opt1 = 0x0 : uint 4 
    opt2 = 0x1

  bitdata OptionData where 
    OptionData = { opt : ChooseOption, val : uint 4 }

Bitdata definitions are not parsers, but rather are used by applying coercions to
already parsed bytes. The following code parses a byte, and then checks that the
first four bits (the most significant four bits) select the correct option. 

.. code-block:: DaeDaLus 

  block
    let odat = UInt8 as? OptionData
    case odat of
      OptionData x ->
        case x.opt of
          opt1 -> ^ x.val
          _    -> Fail "Wrong option"

Note that the coercion may fail if the parsed byte does not contain either
``0x0`` or ``0x1`` in its first four bits. In this case, the parser will
backtrack.

