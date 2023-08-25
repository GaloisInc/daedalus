 {-|
  Name: PHAR-ZIP
  Description: This file contains a Daedalus description of a PHAR/ZIP
  polyglot that composes the files generated from PHAR.ddl and ZIP.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import ZIP

def Main =
  block
    Prefix
    Suffix

def Prefix = ZIPContents
def Suffix = PHARContents
