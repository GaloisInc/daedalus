 {-|
  Name: ZIP-GZ
  Description: This file contains a Daedalus description of a ZIP/GZ
  polyglot that composes the files generated from ZIP.ddl and GZ.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import GZ

def Main =
  block
    Prefix
    Suffix

def Prefix = GZContents
def Suffix = ZIPContents
