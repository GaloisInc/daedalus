 {-|
  Name: ZIP-ISO
  Description: This file contains a Daedalus description of a ZIP/ISO
  polyglot that composes the files generated from ZIP.ddl and ISO.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import ISO

def Main =
  block
    Prefix
    Suffix

def Prefix = ISOContents
def Suffix = ZIPContents
