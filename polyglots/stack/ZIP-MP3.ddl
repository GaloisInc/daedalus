 {-|
  Name: ZIP-MP3
  Description: This file contains a Daedalus description of a ZIP/MP3
  polyglot that composes the files generated from ZIP.ddl and MP3.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import MP3

def Main =
  block
    Prefix
    Suffix

def Prefix = MP3Contents
def Suffix = ZIPContents
