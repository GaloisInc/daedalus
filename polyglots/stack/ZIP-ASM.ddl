 {-|
  Name: ZIP-ASM
  Description: This file contains a Daedalus description of a ZIP/ASM
  polyglot that composes the files generated from ZIP.ddl and ASM.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import ASM

def Main =
  block
    Prefix
    Suffix

def Prefix = ASMContents
def Suffix = ZIPContents
