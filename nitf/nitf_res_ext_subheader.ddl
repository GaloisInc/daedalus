import nitf_lib

def ResExtHeader = {
  Match "RE" ;
  resid = Many 25 BCSA ;
  resver = PosNumber 2 ;

  common = CommonSubheader ;

  -- TODO: implement validation in note on p125

  resshl = UnsignedNumUInt 4 ;
  resshf = Many resshl BCSA ;

  Many Byte 

  -- TODO: refactor above fields into lib
}
