import nitf_lib

-- Wrap a value in a Maybe 

def CallMeMaybe P = { @r = P; ^ just r}

{-- Field parsers --} 

def FHDR = "NITF" 

def FVER = "02.10" 

-- Valid values 01 .. 99 
def CLEVEL ={ 
  @l = UnsignedNum 2; 
  l != 0; 
  ^l; 
}

def STYPE = "BF01" 

def OSTAID = Choose1 { 
  {"0000000000"; MyFail};  -- ill-formed
  Many 10 BCSA; 
}

def FDT = DateTime

def FTITLE = Many 80 ECSA 

def FSCLAS = 'T' | 'S' | 'C' | 'R' | 'U' 

def Digraph = Many (1..) {'A'..'Z'} 

-- TODO: Field must contain valid codes per FIPS PUB 10-4, "XN", or "  "   
-- TODO: Field must be set if any of the following are set: FSCODE, 
--       FSREL, FSDCTP, FSDCDT, FSDCXM, FSDG, FSDGDT, FSCLTX, FSCATP, 
--       FSCAUT, FSCRSN, FSSRDT, and FSCTLN
def FSCLSY = Digraph | "  " 

def DigraphSeq = Choose1 { 
  { @d = Digraph; 
    @ds = Many {' '; $$ = Digraph}; 
    Many ' '; 
    END; 
    ^ just (concat [ [d], ds ]) }; 
  {Many ' '; END; ^ nothing}; 
} 

def FSCODE = Chunk 11 DigraphSeq 

def FSCTLH = Digraph | "  " 

def FSREL = Choose1 { 
  Chunk 20 DigraphSeq
}

-- Unclear how to pad single characters here... 
def FSDCTP = Choose { 
  dd = "DD" ; 
  de = "DE" ; 
  gd = "GD" ; 
  ge = "GE" ; 
  o = "O " ; 
  x = "X " ; 
  none = "  " ; 
} 

-- TODO Add proper date parsing 
def Date2 = Many 8 ECSA 

-- padding again 
def FSDCXM = [' '; ' '; 'X'; '1' .. '8'; ] | 
             ['X'; '1' .. '8'; ' '; ' '; ] |  
             ['X'; '2'; '5'; '1' .. '9'; ] 

def FSDG = 'S' | 'C' | 'R' 

def FSDGDT = Many 8 ECSA 

def FSCLTX = Many 43 ECSA 

def FS_declass = { 
  declass_type = FSDCTP; 
  fsdcdt = Choose1 {
    { declass_type is dd; CallMeMaybe Date2 }; 
    { Many 8 ' '; ^ nothing }; 
  };
  fsdcxm = Choose1 { 
    { declass_type is x; @f = FSDCXM; ^ just f}; 
    { Many 4 ' '; ^ nothing }; 
  }; 
  fsdg = Choose1 { 
    { declass_type is gd | declass_type is ge; 
      @f = FSDG; ^ just f};
    { ' '; ^ nothing }; 
  }; 
  fsdgdt = Choose1 { 
    { declass_type is gd; @d = Date2; ^ just d}; 
    { Many 8 ' '; ^ nothing}; 
  };
  fscltx = Choose1 { 
    { declass_type is de | declass_type is ge; 
      CallMeMaybe (@Many 43 ECSA) }; 
    { Many 43 ' '; ^ nothing} 
  }
} 

def FSC_auth = Choose1 { 
  { CallMeMaybe { 
      type = {'O' | 'D' | 'M' }; 
      auth = Many 40 ECSA; 
    }
  };  
  {' '; Many 40 ECSA; ^ nothing}; 
} 

def FSCRSN = 'A' .. 'G' | ' ' 

def FSSRDT = Many 8 ECSA 
 
def FSCTLN = Many 15 ECSA 

def FSCOP = Many 5 BCSN

def FSCPYS = Many 5 BCSN

def ENCRYP = BCSN 

-- The nonstandard_utf8 case is a magic sequence that isn't defined in the 
-- NITF standard, but is handled in the Hammer parser
def FBKGC = 
  Choose1 { 
    nonstandard_utf8 = Many 3 {0xef; 0xbf; 0xbd}; 
    normal = Many 3 (0x00 .. 0xFF); 
  } 

def ONAME = Many 24 ECSA 

def OPHONE = Many 18 ECSA 

def FL = Choose1 { 
  unknown_length = Many 12 '9';  
  -- TODO put interval bounds here:
  file_length = UnsignedNum 12 ; 
} 

def HL = UnsignedNum 6 

-- TODO: refactor this into code that computes max for some number of UnsignedNums

def ImgLens = {
  lish = BoundedNum 6 439 999998 ;
  li = BoundedPos 10 9999999999
}

-- GraphLens: lengths of graphic segments:
def GraphLens = {
  lssh = BoundedNum 4 258 9999 ;
  seglen = BoundedPos 6 999999
}

def NUMX = "000" 

def LTSH = Many 4 BCSN 
def LT = Many 5 BCSN 

def TextLens = {
  ltsh = BoundedNum 4 282 9999 ;
  lt = BoundedPos 5 999999
}

def DataExtLens = {
  ldsh = BoundedNum 4 200 9999 ;
  ld = BoundedPos 9 999999999
}

def LRESH = Many 4 BCSN 
def LRE = Many 7 BCSN 

def ResExtLens = {
  lresh = BoundedNum 4 200 9999 ;
  lre = BoundedPos 7 9999999
}

def UDHDL = Many 5 BCSN 

def UserData = Choose1 { 
                 none = {"00000"; ^ {} } ; 
                 udhd = {@l = UnsignedNum 5; 
                         overflow = Many 3 BCSN; 
                         data = Many (l - 3) UInt8; }; 
               } 


{-- Main header parser --}

def Header = { 
  FHDR; 
  fver = FVER;
  clevel = CLEVEL; 
  STYPE;
  ostaid = OSTAID;
  fdt = FDT;

  -- check that the file was created after epoch
  @ep = Epoch ;
  OrdDate ep fdt.date ;

  -- check that the file was created no later than the current date
  @today = Today ;
  OrdDate fdt.date today ;

  ftitle = FTITLE;
  fsclas = FSCLAS; 
  fsclsy = FSCLSY; 
  fscode = FSCODE;  
  fsctlh = FSCTLH; 
  fsrel = FSREL; 
  
  fs_declass = FS_declass; 

  fsc_auth = FSC_auth; 

  fscrsn = FSCRSN; 
  fssrdt = FSSRDT; 
  fsctln = FSCTLN;
  fscop = FSCOP; 
  fscpys = FSCPYS; 
  encryp = ENCRYP; 
  fbkgc = FBKGC;
  oname = ONAME; 
  ophone = OPHONE; 
  fl = FL; 
  hl = HL; 

  numi = UnsignedNum 3;
  li = Many numi ImgLens ; 

  nums = UnsignedNum 3; 
  graphlens = Many nums GraphLens ;

  NUMX; 
  @numt = UnsignedNum 3; 
  textlens = Many numt TextLens ; 

  @numdes = UnsignedNum 3; 
  dataextlens = Many numdes DataExtLens ;

  @numres = UnsignedNum 3; 
  resextlens = Many numres ResExtLens ;

  lre = Many numres [LRESH; LRE]; 
  
  udhd = UserData;
  xhd = UserData; 
}
