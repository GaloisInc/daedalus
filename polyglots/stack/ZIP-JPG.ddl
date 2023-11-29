 {-|
  Name: ZIP-JPG
  Description: This file contains a Daedalus description of a ZIP/JPG
  polyglot that composes the files generated from ZIP.ddl and JPG.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import JPG

def Main =
  block
    Prefix
    Suffix

def Prefix = JPGContents
def Suffix = ZIPContents
