 {-|
  Name: ZIP-FLV
  Description: This file contains a Daedalus description of a ZIP/FLV
  polyglot that composes the files generated from ZIP.ddl and FLV.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import FLV

def Main =
  block
    Prefix
    Suffix

def Prefix = FLVContents
def Suffix = ZIPContents
