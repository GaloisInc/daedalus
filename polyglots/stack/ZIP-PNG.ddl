 {-|
  Name: ZIP-PNG
  Description: This file contains a Daedalus description of a ZIP/PNG
  polyglot that composes the files generated from ZIP.ddl and PNG.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import PNG

def Main =
  block
    Prefix
    Suffix

def Prefix = PNGContents
def Suffix = ZIPContents
