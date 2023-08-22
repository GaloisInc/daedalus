 {-|
  Name: ZIP-ELF
  Description: This file contains a Daedalus description of a ZIP/ELF
  polyglot that composes the files generated from ZIP.ddl and ELF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import ELF

def Main =
  block
    Prefix
    Suffix

def Prefix = ELFContents
def Suffix = ZIPContents
