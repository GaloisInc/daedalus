-- module for parsing unicode characters
import Stdlib

-- CharCode c: encoding of (ASCII) character c as a character code
def CharCode c = {
  high = ^0;
  low = ^c;
}

def highBitLow x = (0x80 .&. x) == 0

def ASCIIByte = {
  $$ = UInt8;
  Guard (highBitLow $$)
}

def NonASCIIByte = {
  $$ = UInt8;
  Guard (!(highBitLow $$))
}

-- UTF-8: byte sequences of length 1 <= n <= 4
def UTF8 = Choose {
  utf81 = Bytes1P;
  utf82 = Bytes2P;
  utf83 = Bytes3P;
  utf84 = Bytes4P; -- TODO: refine to check byte values, if needed
  utf86 = Bytes6P; -- not in standard, but occur in the Adobe Glyph List
}

def UTF8Ascii (x : uint 8) : UTF8 = {|
  utf81 = x
|}

def UTF82 (bs : Bytes2) : UTF8 = {|
  utf82 = bs
|}

def UTF84 (bs : Bytes4) : UTF8 = {|
  utf84 = bs
|}

-- UnicodeASCII: parse an ASCII byte as a unicode character
def UnicodeASCII : UTF8 = {|
  utf81 = ASCIIByte
|}

def utf8CharBytes (utfChar: UTF8) = case utfChar of
  utf81 cs1 -> bndBytes1 cs1
  utf82 cs2 -> bndBytes2 cs2
  utf83 cs3 -> bndBytes3 cs3
  utf84 cs4 -> bndBytes4 cs4

-- UnicodeBytes utf8Chars: bytes in the sequence of UTF8 chars uniBytes
def utf8Bytes utf8Chars = for (acc = []; uniChar in utf8Chars) (
  append acc (utf8CharBytes uniChar)
)
