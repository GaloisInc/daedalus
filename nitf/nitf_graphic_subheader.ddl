import nitf_lib

def SY = Match "SY"

def SFmt = Match1 'C'

def GraphicColor = Choose {
  color = Match1 'C' ;
  mono = Match1 'M'
}

def GraphicHeader = {
  SY ;
  sid = Many 10 BCSA ;
  sname = Many 20 ECSA ;

  common = CommonSubheader ;

  Encryp ;

  SFmt ;

  sstruct = UnsignedNum 13 ;

  -- TODO: this defines a global property to check
  sdlvl = UnsignedNum 3 ;

  salvl = AttachmentLvl ;

  sloc = Location ;

  sbnd1 = Location ;

  scolor = GraphicColor ;

  -- TODO: should this semantic value be validated against sbnd1?
  sbnd2 = Location ;

  sres2 = UnsignedNum 2 ;

  -- TODO: refactor this into other subheaders
  sxshdl = Choose {
    notre = @(IsNum 5 0) ;
    taggedrec = BoundedNum 5 3 9741
  } ;

  xssofl = Choose {
    nooverflow = IsNum 3 0 ;
    desseq = PosNumber 3 ;
    omitted = sxshdl is notre ;
  } ;

  sxshd = Choose {
    tre = {
      seq = sxshdl is taggedrec ;
      Many (seq as! uint 64) Byte
    } ;
    ommitted = sxshdl is notre
  } ;
}
