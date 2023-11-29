 {-|
  Name: ZIP-DCM
  Description: This file contains a Daedalus description of a ZIP/DCM
  polyglot that composes the files generated from ZIP.ddl and DCM.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import DCM

def Main =
  block
    Prefix
    Suffix

def Prefix = DCMContents
def Suffix = ZIPContents
