import nitf_lib

def SY = Match "SY"

def SFmt = $['C']

def GraphicColor = First
  color = $['C']
  mono = $['M']

def GraphicHeader = block
  SY
  sid = Many 10 BCSA
  sname = Many 20 ECSA

  common = CommonSubheader

  Encryp

  SFmt

  sstruct = UnsignedNum 13

  -- TODO: this defines a global property to check
  sdlvl = UnsignedNum 3

  salvl = AttachmentLvl

  sloc = Location

  sbnd1 = Location

  scolor = GraphicColor

  -- TODO: should this semantic value be validated against sbnd1?
  sbnd2 = Location

  sres2 = UnsignedNum 2

  -- TODO: refactor this into other subheaders
  sxshd = UserData 9741
