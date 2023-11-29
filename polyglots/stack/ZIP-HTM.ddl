 {-|
  Name: ZIP-HTM
  Description: This file contains a Daedalus description of a ZIP/HTM
  polyglot that composes the files generated from ZIP.ddl and HTM.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import HTM

def Main =
  block
    Prefix
    Suffix

def Prefix = HTMContents
def Suffix = ZIPContents
