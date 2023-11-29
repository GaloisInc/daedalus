 {-|
  Name: PDF-PSD
  Description: This file contains a Daedalus description of a PDF/PSD
  polyglot that composes the files generated from PDF.ddl and PSD.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import PSD

def Main =
  block
    Prefix
    Suffix

def Prefix = PSDContents
def Suffix = PDFContents
