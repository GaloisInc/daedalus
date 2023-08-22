 {-|
  Name: PDF-CAB
  Description: This file contains a Daedalus description of a PDF/CAB
  polyglot that composes the files generated from PDF.ddl and CAB.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import CAB

def Main =
  block
    Prefix
    Suffix

def Prefix = CABContents
def Suffix = PDFContents
