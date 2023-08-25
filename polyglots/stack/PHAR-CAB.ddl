 {-|
  Name: PHAR-CAB
  Description: This file contains a Daedalus description of a PHAR/CAB
  polyglot that composes the files generated from PHAR.ddl and CAB.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import CAB

def Main =
  block
    Prefix
    Suffix

def Prefix = CABContents
def Suffix = PHARContents
