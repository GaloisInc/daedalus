 {-|
  Name: ZIP-TIF
  Description: This file contains a Daedalus description of a ZIP/TIF
  polyglot that composes the files generated from ZIP.ddl and TIF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import TIF

def Main =
  block
    Prefix
    Suffix

def Prefix = TIFContents
def Suffix = ZIPContents
