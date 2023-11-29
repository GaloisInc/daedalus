 {-|
  Name: ZIP-ARJ
  Description: This file contains a Daedalus description of a ZIP/ARJ
  polyglot that composes the files generated from ZIP.ddl and ARJ.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import ARJ

def Main =
  block
    Prefix
    Suffix

def Prefix = ARJContents
def Suffix = ZIPContents
