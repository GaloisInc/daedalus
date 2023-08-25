 {-|
  Name: PHAR-ELF
  Description: This file contains a Daedalus description of a PHAR/ELF
  polyglot that composes the files generated from PHAR.ddl and ELF.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import ELF

def Main =
  block
    Prefix
    Suffix

def Prefix = ELFContents
def Suffix = PHARContents
