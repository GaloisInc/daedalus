 {-|
  Name: ZIP-AVI
  Description: This file contains a Daedalus description of a ZIP/AVI
  polyglot that composes the files generated from ZIP.ddl and AVI.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import AVI

def Main =
  block
    Prefix
    Suffix

def Prefix = AVIContents
def Suffix = ZIPContents
