 {-|
  Name: PDF-BMP
  Description: This file contains a Daedalus description of a PDF/BMP
  polyglot that composes the files generated from PDF.ddl and BMP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import BMP

def Main =
  block
    Prefix
    Suffix

def Prefix = BMPContents
def Suffix = PDFContents
