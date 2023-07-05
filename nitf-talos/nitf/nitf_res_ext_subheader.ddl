import nitf_lib

def ResExtHeader lresh =
  block
    Match "RE"
    resid = Many 25 BCSA
    resver = PosNumber 2

    common = CommonSubheader

    -- TODO: implement validation in note on p125

    resshl = UnsignedNum 4 as? uint 64
    resshf = Many resshl BCSA

    -- FIXME: the 200 is the size of the above, but we should check it.
    Many (lresh - 200) Byte

    -- TODO: refactor above fields into lib
