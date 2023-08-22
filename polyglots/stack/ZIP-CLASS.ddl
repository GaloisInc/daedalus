 {-|
  Name: ZIP-CLASS
  Description: This file contains a Daedalus description of a ZIP/CLASS
  polyglot that composes the files generated from ZIP.ddl and CLASS.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import CLASS

def Main =
  block
    Prefix
    Suffix

def Prefix = CLASSContents
def Suffix = ZIPContents
