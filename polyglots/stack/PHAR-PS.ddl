 {-|
  Name: PHAR-PS
  Description: This file contains a Daedalus description of a PHAR/PS
  polyglot that composes the files generated from PHAR.ddl and PS.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import PS

def Main =
  block
    Prefix
    Suffix

def Prefix = PSContents
def Suffix = PHARContents
