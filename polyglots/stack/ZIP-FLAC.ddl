 {-|
  Name: ZIP-FLAC
  Description: This file contains a Daedalus description of a ZIP/FLAC
  polyglot that composes the files generated from ZIP.ddl and FLAC.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import FLAC

def Main =
  block
    Prefix
    Suffix

def Prefix = FLACContents
def Suffix = ZIPContents
