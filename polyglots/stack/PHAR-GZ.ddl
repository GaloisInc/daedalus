 {-|
  Name: PHAR-GZ
  Description: This file contains a Daedalus description of a PHAR/GZ
  polyglot that composes the files generated from PHAR.ddl and GZ.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import GZ

def Main =
  block
    Prefix
    Suffix

def Prefix = GZContents
def Suffix = PHARContents
