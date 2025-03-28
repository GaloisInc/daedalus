 {-|
  Name: ZIP-BZ2
  Description: This file contains a Daedalus description of a ZIP/BZ2
  polyglot that composes the files generated from ZIP.ddl and BZ2.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import BZ2

def Main =
  block
    Prefix
    Suffix

def Prefix = BZ2Contents
def Suffix = ZIPContents
