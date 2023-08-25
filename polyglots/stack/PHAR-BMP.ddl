 {-|
  Name: PHAR-BMP
  Description: This file contains a Daedalus description of a PHAR/BMP
  polyglot that composes the files generated from PHAR.ddl and BMP.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import BMP

def Main =
  block
    Prefix
    Suffix

def Prefix = BMPContents
def Suffix = PHARContents
