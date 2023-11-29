 {-|
  Name: ZIP-Y4M
  Description: This file contains a Daedalus description of a ZIP/Y4M
  polyglot that composes the files generated from ZIP.ddl and Y4M.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import Y4M

def Main =
  block
    Prefix
    Suffix

def Prefix = Y4MContents
def Suffix = ZIPContents
