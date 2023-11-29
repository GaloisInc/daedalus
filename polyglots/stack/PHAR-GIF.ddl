 {-|
  Name: PHAR-GIF
  Description: This file contains a Daedalus description of a PHAR/GIF
  polyglot that composes the files generated from PHAR.ddl and GIF.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import GIF

def Main =
  block
    Prefix
    Suffix

def Prefix = GIFContents
def Suffix = PHARContents
