 {-|
  Name: ZIP-SHA256
  Description: This file contains a Daedalus description of a ZIP/SHA256
  polyglot that composes the files generated from ZIP.ddl and SHA256.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import SHA256

def Main =
  block
    Prefix
    Suffix

def Prefix = SHA256Contents
def Suffix = ZIPContents
