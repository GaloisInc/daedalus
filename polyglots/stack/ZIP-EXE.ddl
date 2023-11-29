 {-|
  Name: ZIP-EXE
  Description: This file contains a Daedalus description of a ZIP/EXE
  polyglot that composes the files generated from ZIP.ddl and EXE.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import EXE

def Main =
  block
    Prefix
    Suffix

def Prefix = EXEContents
def Suffix = ZIPContents
