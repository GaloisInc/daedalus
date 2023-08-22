 {-|
  Name: ZIP-BMP
  Description: This file contains a Daedalus description of a ZIP/BMP
  polyglot that composes the files generated from ZIP.ddl and BMP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import BMP

def Main =
  block
    Prefix
    Suffix

def Prefix = BMPContents
def Suffix = ZIPContents
