 {-|
  Name: PDF-ARJ
  Description: This file contains a Daedalus description of a PDF/ARJ
  polyglot that composes the files generated from PDF.ddl and ARJ.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import ARJ

def Main =
  block
    Prefix
    Suffix

def Prefix = ARJContents
def Suffix = PDFContents
