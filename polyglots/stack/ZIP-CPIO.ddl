 {-|
  Name: ZIP-CPIO
  Description: This file contains a Daedalus description of a ZIP/CPIO
  polyglot that composes the files generated from ZIP.ddl and CPIO.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import CPIO

def Main =
  block
    Prefix
    Suffix

def Prefix = CPIOContents
def Suffix = ZIPContents
