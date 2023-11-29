 {-|
  Name: PHAR-DCM
  Description: This file contains a Daedalus description of a PHAR/DCM
  polyglot that composes the files generated from PHAR.ddl and DCM.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import DCM

def Main =
  block
    Prefix
    Suffix

def Prefix = DCMContents
def Suffix = PHARContents
