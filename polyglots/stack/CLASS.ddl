{-|
  Name: CLASS
  Description: This file contains a Daedalus description of the contents of
  a CLASS file for Talos to generate.  Use `Match [...]` to force Talos to
  produce a specific CLASS file.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

def CLASSContents = Match [
0xca, 0xfe, 0xba, 0xbe, 0x00, 0x03, 0x00, 0x2d, 0x00, 0x08, 0x07, 0x00, 0x02, 0x01, 0x00, 0x04,
0x6d, 0x69, 0x6e, 0x69, 0x07, 0x00, 0x04, 0x01, 0x00, 0x10, 0x6a, 0x61, 0x76, 0x61, 0x2f, 0x6c,
0x61, 0x6e, 0x67, 0x2f, 0x4f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x01, 0x00, 0x04, 0x6d, 0x61, 0x69,
0x6e, 0x01, 0x00, 0x04, 0x43, 0x6f, 0x64, 0x65, 0x01, 0x00, 0x16, 0x28, 0x5b, 0x4c, 0x6a, 0x61,
0x76, 0x61, 0x2f, 0x6c, 0x61, 0x6e, 0x67, 0x2f, 0x53, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x3b, 0x29,
0x56, 0x00, 0x01, 0x00, 0x01, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x09, 0x00,
0x05, 0x00, 0x07, 0x00, 0x01, 0x00, 0x06, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00, 0x01, 0x00,
0x00, 0x00, 0x01, 0xb1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,      
]
