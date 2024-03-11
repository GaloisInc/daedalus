{-|
  Name: ZIP

  Description: This file contains a Daedalus description Java's conditional
  cavity characteristics.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

-- We use an unreferenced string object to create a cavity in ZIP.
-- Constraints: cavity cannot contain unescaped unbalanced parentheses.

def ZIPCavityStartDelimiter = Match ""
def ZIPCavityEndDelimiter   = Match ""

def ZIPContent = Match [
 0x50, 0x4b, 0x03, 0x04, 0x0a, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x23, 0x8e,
 0x5a, 0x6b, 0x05, 0x00, 0x00, 0x00, 0x05, 0x00,  0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x7a, 0x69,
 0x70, 0x2e, 0x74, 0x78, 0x74, 0x5a, 0x49, 0x50,  0x0d, 0x0a, 0x50, 0x4b, 0x01, 0x02, 0x1f, 0x00,
 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x23, 0x8e, 0x5a, 0x6b, 0x05, 0x00,
 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x07, 0x00,  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  0x7a, 0x69, 0x70, 0x2e, 0x74, 0x78, 0x74, 0x50,
 0x4b, 0x05, 0x06, 0x00, 0x00, 0x00, 0x00, 0x01,  0x00, 0x01, 0x00, 0x35, 0x00, 0x00, 0x00, 0x2a,
 0x00, 0x00, 0x00, 0x00, 0x00,            
]