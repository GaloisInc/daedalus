 {-|
  Name: PHAR-RAR
  Description: This file contains a Daedalus description of a PHAR/RAR
  polyglot that composes the files generated from PHAR.ddl and RAR.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import RAR

def Main =
  block
    Prefix
    Suffix

def Prefix = RARContents
def Suffix = PHARContents
