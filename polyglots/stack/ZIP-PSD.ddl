 {-|
  Name: ZIP-PSD
  Description: This file contains a Daedalus description of a ZIP/PSD
  polyglot that composes the files generated from ZIP.ddl and PSD.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import PSD

def Main =
  block
    Prefix
    Suffix

def Prefix = PSDContents
def Suffix = ZIPContents
