{-|
  Name: RAR
  Description: This file contains a Daedalus description of the contents of
  a RAR file for Talos to generate.  Use `Match [...]` to force Talos to
  produce a specific RAR file.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

def RARContents = Match [
0x52, 0x61, 0x72, 0x21, 0x1a, 0x07, 0x01, 0x00, 0x33, 0x92, 0xb5, 0xe5, 0x0a, 0x01, 0x05, 0x06,
0x00, 0x05, 0x01, 0x01, 0x80, 0x80, 0x00, 0xde, 0x7f, 0x87, 0xfb, 0x24, 0x02, 0x03, 0x0b, 0x84,
0x00, 0x04, 0x84, 0x00, 0x20, 0x37, 0x04, 0x26, 0xef, 0x80, 0x00, 0x00, 0x08, 0x72, 0x61, 0x72,
0x35, 0x2e, 0x74, 0x78, 0x74, 0x0a, 0x03, 0x02, 0xd9, 0x9c, 0x57, 0x3c, 0x2a, 0xce, 0xd5, 0x01,
0x52, 0x41, 0x52, 0x35, 0x1d, 0x77, 0x56, 0x51, 0x03, 0x05, 0x04, 0x00,    
]
