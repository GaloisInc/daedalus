 {-|
  Name: ZIP-AR
  Description: This file contains a Daedalus description of a ZIP/AR
  polyglot that composes the files generated from ZIP.ddl and AR.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import AR

def Main =
  block
    Prefix
    Suffix

def Prefix = ARContents
def Suffix = ZIPContents
