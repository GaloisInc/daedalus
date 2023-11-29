 {-|
  Name: ZIP-CAB
  Description: This file contains a Daedalus description of a ZIP/CAB
  polyglot that composes the files generated from ZIP.ddl and CAB.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import CAB

def Main =
  block
    Prefix
    Suffix

def Prefix = CABContents
def Suffix = ZIPContents
