 {-|
  Name: ZIP-WEBM
  Description: This file contains a Daedalus description of a ZIP/WEBM
  polyglot that composes the files generated from ZIP.ddl and WEBM.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import WEBM

def Main =
  block
    Prefix
    Suffix

def Prefix = WEBMContents
def Suffix = ZIPContents
