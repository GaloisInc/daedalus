 {-|
  Name: PDF-TIF
  Description: This file contains a Daedalus description of a PDF/TIF
  polyglot that composes the files generated from PDF.ddl and TIF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import TIF

def Main =
  block
    Prefix
    Suffix

def Prefix = TIFContents
def Suffix = PDFContents
