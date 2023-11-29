 {-|
  Name: ZIP-ICO
  Description: This file contains a Daedalus description of a ZIP/ICO
  polyglot that composes the files generated from ZIP.ddl and ICO.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import ICO

def Main =
  block
    Prefix
    Suffix

def Prefix = ICOContents
def Suffix = ZIPContents
