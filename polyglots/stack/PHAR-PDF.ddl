 {-|
  Name: PHAR-PDF
  Description: This file contains a Daedalus description of a PHAR/PDF
  polyglot that composes the files generated from PHAR.ddl and PDF.ddl.
  Note that this does not include the Phar signature, which must be
  generated and appended as a post-processing step.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PHAR
import PDF

def Main =
  block
    Prefix
    Suffix

def Prefix = PDFContents
def Suffix = PHARContents
