 {-|
  Name: PHAR-MP4
  Description: This file contains a Daedalus description of a PHAR/MP4
  polyglot that composes the files generated from PHAR.ddl and MP4.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import MP4

def Main =
  block
    Prefix
    Suffix

def Prefix = MP4Contents
def Suffix = PHARContents
