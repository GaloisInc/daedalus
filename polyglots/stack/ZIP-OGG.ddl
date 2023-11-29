 {-|
  Name: ZIP-OGG
  Description: This file contains a Daedalus description of a ZIP/OGG
  polyglot that composes the files generated from ZIP.ddl and OGG.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import OGG

def Main =
  block
    Prefix
    Suffix

def Prefix = OGGContents
def Suffix = ZIPContents
