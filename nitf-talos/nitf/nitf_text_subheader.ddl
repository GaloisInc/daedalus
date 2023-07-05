import nitf_lib

def TE = Match "TE"

def TextId = Many 7 (AlphaNum <| $[' '])

def TxTitl = Many 80 ECSA

def TxtFmt = First
  usmtf = @Match "MTF"
  bcs   = @Match "STA"
  ecs   = @Match "UT1"
  u8s   = @Match "U8S"

-- def TxShDL = First
--   no_txshdl  = IsNum 5 0
--   has_txshdl = BoundedNum 5 3 9717

def TextHeader = block
  @start = TalosOffset
  TE
  textid = TextId
  txtalvl = AttachmentLvl
  txtdt = PartialDateTime
  txtitl = TxTitl

  common = CommonSubheader

  Encryp

  txtfmt = TxtFmt

  txshd = UserData 9717 -- BCSA  
  @end = TalosOffset
  talos_fixup_bsize = ^ (end - start)
