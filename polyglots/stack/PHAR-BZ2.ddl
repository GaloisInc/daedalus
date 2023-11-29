 {-|
  Name: PHAR-BZ2
  Description: This file contains a Daedalus description of a PHAR/BZ2
  polyglot that composes the files generated from PHAR.ddl and BZ2.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import BZ2

def Main =
  block
    Prefix
    Suffix

def Prefix = BZ2Contents
def Suffix = PHARContents
