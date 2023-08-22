 {-|
  Name: ZIP-XZ
  Description: This file contains a Daedalus description of a ZIP/XZ
  polyglot that composes the files generated from ZIP.ddl and XZ.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import XZ

def Main =
  block
    Prefix
    Suffix

def Prefix = XZContents
def Suffix = ZIPContents
