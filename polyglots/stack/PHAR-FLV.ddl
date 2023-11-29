 {-|
  Name: PHAR-FLV
  Description: This file contains a Daedalus description of a PHAR/FLV
  polyglot that composes the files generated from PHAR.ddl and FLV.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import FLV

def Main =
  block
    Prefix
    Suffix

def Prefix = FLVContents
def Suffix = PHARContents
