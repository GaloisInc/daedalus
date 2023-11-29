 {-|
  Name: PHAR-JPG
  Description: This file contains a Daedalus description of a PHAR/JPG
  polyglot that composes the files generated from PHAR.ddl and JPG.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import JPG

def Main =
  block
    Prefix
    Suffix

def Prefix = JPGContents
def Suffix = PHARContents
