 {-|
  Name: ZIP-RAR
  Description: This file contains a Daedalus description of a ZIP/RAR
  polyglot that composes the files generated from ZIP.ddl and RAR.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import RAR

def Main =
  block
    Prefix
    Suffix

def Prefix = RARContents
def Suffix = ZIPContents
