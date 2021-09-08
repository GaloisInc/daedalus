import nitf_lib

def TE = Match "TE"

def TextId = Many 7 (AlphaNum | Match1 ' ')

def TxTitl = Many 80 ECSA

def TxtFmt = Choose {
  usmtf = @Match "MTF" ;
  bcs = @Match "STA" ;
  ecs = @Match "UT1" ;
  u8s = @Match "U8S" ;
}

def TxShDL =
  (IsNum 5 0)
| (BoundedNum 5 3 9717)

def TextHeader = {
  TE ;
  textid = TextId ;
  txtalvl = AttachmentLvl ;
  txtdt = PartialDateTime ;
  txtitl = TxTitl ;

  common = CommonSubheader ;

  Encryp ;

  txtfmt = TxtFmt ;

  txshdl = TxShDL as! uint 64;

  Choose { -- NOTE-MODERN: seems overlapping but not harmful
    txsofl = {
      UnsignedNum 3 ;
    } ;
    omitted = Guard (txshdl == 0);
  } ;

  txshd = Many (txshdl - 3) BCSA ;
}
