 {-|
  Name: PHAR-7Z
  Description: This file contains a Daedalus description of a PHAR/7Z
  polyglot that composes the files generated from PHAR.ddl and 7Z.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import 7Z

def Main =
  block
    Prefix
    Suffix

def Prefix = 7ZContents
def Suffix = PHARContents
