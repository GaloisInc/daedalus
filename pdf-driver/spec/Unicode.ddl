-- module for parsing unicode characters
import Stdlib

-- CharCode c: encoding of (ASCII) character c as a character code
def CharCode c = {
  high = ^0;
  low = ^c;
}

-- UnicodeString U s: the unicode representation of character string s
def UnicodeString Unicoder s = map (c in s) 
  (Unicoder (CharCode c))

-- UTF-8: byte sequences of length 1 <= n <= 4
def UTF8 = Choose {
  utf81 = Bytes1;
  utf82 = Bytes2;
  utf83 = Bytes3;
  utf84 = Bytes4;
}

def CharCodeToUnicode Ccode : UTF8 = {
  @ccode = Ccode;
  {| utf82 = {
       second = ^ccode.high;
       rest1 = ^ccode.low;
     }
  |}
}

def utf8CharBytes (utfChar: UTF8) = case utfChar of
  utf81 cs1 -> bndBytes1 cs1
  utf82 cs2 -> bndBytes2 cs2
  utf83 cs3 -> bndBytes3 cs3
  utf84 cs4 -> bndBytes4 cs4

-- UnicodeBytes uniBytes: bytes in the sequence of UTF8 chars uniBytes
def utf8Bytes utf8Chars = for (acc = []; uniChar in utf8Chars) (
  append acc (utf8CharBytes uniChar)
)
