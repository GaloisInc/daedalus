 {-|
  Name: ZIP-JP2
  Description: This file contains a Daedalus description of a ZIP/JP2
  polyglot that composes the files generated from ZIP.ddl and JP2.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import JP2

def Main =
  block
    Prefix
    Suffix

def Prefix = JP2Contents
def Suffix = ZIPContents
