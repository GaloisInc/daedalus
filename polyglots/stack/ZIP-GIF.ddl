 {-|
  Name: ZIP-GIF
  Description: This file contains a Daedalus description of a ZIP/GIF
  polyglot that composes the files generated from ZIP.ddl and GIF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import GIF

def Main =
  block
    Prefix
    Suffix

def Prefix = GIFContents
def Suffix = ZIPContents
