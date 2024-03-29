{-|
  Name: ELF
  Description: This file contains a Daedalus description of the contents of
  a ELF file for Talos to generate.  Use `Match [...]` to force Talos to
  produce a specific ELF file.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

def ELFContents = Match [
0x7f, 0x45, 0x4c, 0x46, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x02, 0x00, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x08, 0x40, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x34, 0x00, 0x20, 0x00, 0x01, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x08,
0x70, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0xbb, 0x2a, 0x00, 0x00, 0x00, 0xb8, 0x01, 0x00, 0x00, 0x00, 0xcd, 0x80, 0x00, 0x00, 0x00, 0x00,
]
