 {-|
  Name: ZIP-MKV
  Description: This file contains a Daedalus description of a ZIP/MKV
  polyglot that composes the files generated from ZIP.ddl and MKV.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import MKV

def Main =
  block
    Prefix
    Suffix

def Prefix = MKVContents
def Suffix = ZIPContents
