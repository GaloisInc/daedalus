 {-|
  Name: PHAR-FLAC
  Description: This file contains a Daedalus description of a PHAR/FLAC
  polyglot that composes the files generated from PHAR.ddl and FLAC.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import FLAC

def Main =
  block
    Prefix
    Suffix

def Prefix = FLACContents
def Suffix = PHARContents
