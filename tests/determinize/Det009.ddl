-- testing grammar call with parameters from JpegBasics.ddl

def Main = block
  @e = Endian
  IFDEntry e

def Endian = First
  LE = @Match "II"
  BE = @Match "MM"

def IFDEntry e = First
  imageWidth  = NumericTag e 0x100
  imageHeight = NumericTag e 0x101
  -- bits per sample
  compression = NumericTag e 0x103    -- 1 = none, 6 = jpeg
  photometricInterpretation = NumericTag e 0x106

  samplesPerPixel = NumericTag e 0x115
  rowsPerStrip    = NumericTag e 0x116

  unknown     = IFDUnknown e

def Tag e x = Word16 e == x is true
def NumericTag e x = block
  Tag e x
  Unsgned e

def Unsgned e = block
  let format = Word16 e
  -- let items  = Word32 e
  -- items == 1 is true
  -- case format of
  --   1 -> block $$ = UInt8    as uint 64; Many 3 UInt8
  --   3 -> block $$ = Word16 e as uint 64; Many 2 UInt8
  --   4 -> Word32 e as uint 64

def IFDUnknown e = block
  tag    = Word16 e
  format = Word16 e as uint 64
  -- items  = Word32 e as uint 64
  -- data   = Word32 e


--------------------------------------------------------------------------------
-- Misc. utilities

def jn (e : Endian) x y =
  case e of
    BE -> x # y
    LE -> y # x

def Word16 e = jn e UInt8 UInt8
def Word32 e = jn e (Word16 e) (Word16 e)