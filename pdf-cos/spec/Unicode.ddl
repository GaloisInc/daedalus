-- module for parsing unicode characters
import Stdlib
import PdfValue

---- conversions ---------------------------------------------------------------

-- CodePoint = the numeric values making up codespace, Unicode comprises 1,114,112 code points,
-- we represent code points with 'uint 32'.

def ParseHexUTF16_BE : uint 32 = {
  @p1 = numBase 16 (Many 4 HexDigit) as! uint 32;
  -- @p2 = Optional (Many 4 HexDigit) -- FIXME: implement
  ^ p1 
} 


---- UTF-8 oriented definitions: -----------------------------------------------

-- CharCode c: encoding of (ASCII) character c as a character code
def CharCode c = {
  high = ^0;
  low = ^c;
}

def highBitIsSet (x : uint 8) = bitIsSet8 x 7

def ASCIIByte = {
  $$ = UInt8;
  Guard (!(highBitIsSet $$))
}

def NonASCIIByte = {
  $$ = UInt8;
  Guard (highBitIsSet $$)
}

-- UTF-8: byte sequences of length 1 <= n <= 4
def UTF8 (bs1 : bytes1)
  (bs2 : bytes2)
  (bs3 : bytes3)
  (bs4 : bytes4) = Choose {
  utf81 = bs1;
  utf82 = bs2;
  utf83 = bs3;
  utf84 = bs4; -- TODO: refine to check byte values, if needed
}

def mkUTF81 (bs : bytes1) : UTF8 = {|
  utf81 = bs
|}

def mkUTF82 (bs : bytes2) : UTF8 = {|
  utf82 = bs
|}

def mkUTF83 (bs : bytes3) : UTF8 = {|
  utf83 = bs
|}

def mkUTF84 (bs : bytes4) : UTF8 = {|
  utf84 = bs
|}

def UTF8AsciiP = {
  @b = ASCIIByte;
  mkUTF81 (bytes1 b)
}

def utf8CharBytes (utfChar: UTF8) = case utfChar of
  utf81 cs1 -> bndBytes1 cs1
  utf82 cs2 -> bndBytes2 cs2
  utf83 cs3 -> bndBytes3 cs3
  utf84 cs4 -> bndBytes4 cs4

-- UnicodeBytes utf8Chars: bytes in the sequence of UTF8 chars uniBytes
def utf8Bytes utf8Chars = concat
  (map (uniChar in utf8Chars) (utf8CharBytes uniChar))

def unicodeTailHOBits : uint 8 = 0x80

-- trunc8: truncate a two-byte value to one byte
def trunc8 (x : uint 16) : uint 8 = (0 : uint 8) <# x

def shiftTail (x : uint 16) (numCells : uint 64) : uint 8 =
  trunc8 (x >> (numCells * 6))

def pointTail1 (x : uint 16) = bytes1
  (unicodeTailHOBits .|. (shiftTail x 0))

def pointTail2 (x : uint 16) = bytes2
  (unicodeTailHOBits .|. (shiftTail x 1))
  (pointTail1 (x .&. 0x3F))

def pointTail3 (x : uint 16) = bytes3
  (unicodeTailHOBits .|. (shiftTail x 2))
  (pointTail2 (x .&. 0x7FF))

-- unicodePoint: construct a code point from a two-byte value
-- DBG:
def unicodePoint (x : uint 16) : UTF8 =
  if x <= 0x7F then mkUTF81
    (bytes1 (trunc8 x))
  else if x <= 0x07FF then mkUTF82
    (bytes2 (0xC0 .|. (shiftTail x 1)) (pointTail1 (x .&. 0x3F)))
  else if x <= 0xFFFF then mkUTF83
    (bytes3 (0xE0 .|. (shiftTail x 2)) (pointTail2 (x .&. 0xFFF)))
  else -- TODO: remove this case and rebuild
    mkUTF81 (bytes1 '.')

def unicodePoint0 (x : uint 16) : UTF8 = mkUTF81 (bytes1 '.')
