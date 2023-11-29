{-|
  Name: WEBP
  Description: This file contains a Daedalus description of the contents of
  a WEBP file for Talos to generate.  Use `Match [...]` to force Talos to
  produce a specific WEBP file.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

def WEBPContents = Match [
0x52, 0x49, 0x46, 0x46, 0x3a, 0x00, 0x00, 0x00, 0x57, 0x45, 0x42, 0x50, 0x56, 0x50, 0x38, 0x4c,
0x2d, 0x00, 0x00, 0x00, 0x2f, 0x10, 0x80, 0x01, 0x00, 0x0f, 0x30, 0xff, 0xf3, 0x3f, 0xff, 0xf3,
0x1f, 0x78, 0x50, 0x13, 0xc9, 0x6a, 0x35, 0x27, 0x0a, 0x21, 0x94, 0xc8, 0x82, 0x24, 0x65, 0x64,
0x7d, 0x85, 0xa4, 0x7e, 0x36, 0x22, 0xfa, 0x1f, 0x12, 0x7f, 0x62, 0x81, 0x0f, 0x72, 0x61, 0x0f,
0x09, 0x00,              
]
