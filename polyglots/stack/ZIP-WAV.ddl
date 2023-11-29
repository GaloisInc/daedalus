 {-|
  Name: ZIP-WAV
  Description: This file contains a Daedalus description of a ZIP/WAV
  polyglot that composes the files generated from ZIP.ddl and WAV.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import WAV

def Main =
  block
    Prefix
    Suffix

def Prefix = WAVContents
def Suffix = ZIPContents
