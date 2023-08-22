 {-|
  Name: ZIP-TAR
  Description: This file contains a Daedalus description of a ZIP/TAR
  polyglot that composes the files generated from ZIP.ddl and TAR.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import TAR

def Main =
  block
    Prefix
    Suffix

def Prefix = TARContents
def Suffix = ZIPContents
