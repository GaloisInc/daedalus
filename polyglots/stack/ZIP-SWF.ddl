 {-|
  Name: ZIP-SWF
  Description: This file contains a Daedalus description of a ZIP/SWF
  polyglot that composes the files generated from ZIP.ddl and SWF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import SWF

def Main =
  block
    Prefix
    Suffix

def Prefix = SWFContents
def Suffix = ZIPContents
