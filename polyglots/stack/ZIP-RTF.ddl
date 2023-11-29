 {-|
  Name: ZIP-RTF
  Description: This file contains a Daedalus description of a ZIP/RTF
  polyglot that composes the files generated from ZIP.ddl and RTF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import RTF

def Main =
  block
    Prefix
    Suffix

def Prefix = RTFContents
def Suffix = ZIPContents
