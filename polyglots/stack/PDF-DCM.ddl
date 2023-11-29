 {-|
  Name: PDF-DCM
  Description: This file contains a Daedalus description of a PDF/DCM
  polyglot that composes the files generated from PDF.ddl and DCM.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import DCM

def Main =
  block
    Prefix
    Suffix

def Prefix = DCMContents
def Suffix = PDFContents
