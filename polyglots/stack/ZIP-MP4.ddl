 {-|
  Name: ZIP-MP4
  Description: This file contains a Daedalus description of a ZIP/MP4
  polyglot that composes the files generated from ZIP.ddl and MP4.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import MP4

def Main =
  block
    Prefix
    Suffix

def Prefix = MP4Contents
def Suffix = ZIPContents
