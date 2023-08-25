 {-|
  Name: PHAR-ICO
  Description: This file contains a Daedalus description of a PHAR/ICO
  polyglot that composes the files generated from PHAR.ddl and ICO.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import ICO

def Main =
  block
    Prefix
    Suffix

def Prefix = ICOContents
def Suffix = PHARContents
