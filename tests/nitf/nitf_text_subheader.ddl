import nitf_lib

def TE = "TE"

def TextId = Many 7 (AlphaNum | ' ')

def TxTitl = Many 80 ECSA

def TxtFmt = Choose {
  usmtf = @"MTF" ;
  bcs = @"STA" ;
  ecs = @"UT1" ;
  u8s = @"U8S" ;
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

  txshdl = TxShDL ;

  Choose {
    txsofl = {
      UnsignedNum 3 ;
    } ;
    omitted = txshdl == 0;
  } ;

  txshd = Many (txshdl - 3) BCSA ;
}
