 {-|
  Name: ZIP-PS
  Description: This file contains a Daedalus description of a ZIP/PS
  polyglot that composes the files generated from ZIP.ddl and PS.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import PS

def Main =
  block
    Prefix
    Suffix

def Prefix = PSContents
def Suffix = ZIPContents
