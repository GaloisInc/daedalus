 {-|
  Name: PDF-AR
  Description: This file contains a Daedalus description of a PDF/AR
  polyglot that composes the files generated from PDF.ddl and AR.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import AR

def Main =
  block
    Prefix
    Suffix

def Prefix = ARContents
def Suffix = PDFContents
