 {-|
  Name: PHAR-CPIO
  Description: This file contains a Daedalus description of a PHAR/CPIO
  polyglot that composes the files generated from PHAR.ddl and CPIO.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import CPIO

def Main =
  block
    Prefix
    Suffix

def Prefix = CPIOContents
def Suffix = PHARContents
