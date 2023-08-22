 {-|
  Name: ZIP-WEBP
  Description: This file contains a Daedalus description of a ZIP/WEBP
  polyglot that composes the files generated from ZIP.ddl and WEBP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import WEBP

def Main =
  block
    Prefix
    Suffix

def Prefix = WEBPContents
def Suffix = ZIPContents
