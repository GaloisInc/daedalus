 {-|
  Name: ZIP-WAD
  Description: This file contains a Daedalus description of a ZIP/WAD
  polyglot that composes the files generated from ZIP.ddl and WAD.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import WAD

def Main =
  block
    Prefix
    Suffix

def Prefix = WADContents
def Suffix = ZIPContents
