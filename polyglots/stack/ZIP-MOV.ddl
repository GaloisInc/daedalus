 {-|
  Name: ZIP-MOV
  Description: This file contains a Daedalus description of a ZIP/MOV
  polyglot that composes the files generated from ZIP.ddl and MOV.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import MOV

def Main =
  block
    Prefix
    Suffix

def Prefix = MOVContents
def Suffix = ZIPContents
