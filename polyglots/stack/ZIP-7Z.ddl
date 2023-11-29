 {-|
  Name: ZIP-7Z
  Description: This file contains a Daedalus description of a ZIP/7Z
  polyglot that composes the files generated from ZIP.ddl and 7Z.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import 7Z

def Main =
  block
    Prefix
    Suffix

def Prefix = 7ZContents
def Suffix = ZIPContents
