 {-|
  Name: PHAR-TAR
  Description: This file contains a Daedalus description of a PHAR/TAR
  polyglot that composes the files generated from PHAR.ddl and TAR.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import TAR

def Main =
  block
    Prefix
    Suffix

def Prefix = TARContents
def Suffix = PHARContents
