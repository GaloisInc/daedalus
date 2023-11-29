 {-|
  Name: PHAR-AR
  Description: This file contains a Daedalus description of a PHAR/AR
  polyglot that composes the files generated from PHAR.ddl and AR.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import AR

def Main =
  block
    Prefix
    Suffix

def Prefix = ARContents
def Suffix = PHARContents
