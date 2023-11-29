 {-|
  Name: PDF-PS
  Description: This file contains a Daedalus description of a PDF/PS
  polyglot that composes the files generated from PDF.ddl and PS.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import PS

def Main =
  block
    Prefix
    Suffix

def Prefix = PSContents
def Suffix = PDFContents
