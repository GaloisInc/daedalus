 {-|
  Name: ZIP-MACHO
  Description: This file contains a Daedalus description of a ZIP/MACHO
  polyglot that composes the files generated from ZIP.ddl and MACHO.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import MACHO

def Main =
  block
    Prefix
    Suffix

def Prefix = MACHOContents
def Suffix = ZIPContents
