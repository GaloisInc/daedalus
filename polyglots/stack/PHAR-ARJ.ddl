 {-|
  Name: PHAR-ARJ
  Description: This file contains a Daedalus description of a PHAR/ARJ
  polyglot that composes the files generated from PHAR.ddl and ARJ.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import ARJ

def Main =
  block
    Prefix
    Suffix

def Prefix = ARJContents
def Suffix = PHARContents
