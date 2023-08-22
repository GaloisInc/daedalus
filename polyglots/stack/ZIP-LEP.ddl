 {-|
  Name: ZIP-LEP
  Description: This file contains a Daedalus description of a ZIP/LEP
  polyglot that composes the files generated from ZIP.ddl and LEP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import LEP

def Main =
  block
    Prefix
    Suffix

def Prefix = LEPContents
def Suffix = ZIPContents
