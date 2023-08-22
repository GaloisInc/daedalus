 {-|
  Name: PDF-PNG
  Description: This file contains a Daedalus description of a PDF/PNG
  polyglot that composes the files generated from PDF.ddl and PNG.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import PNG

def Main =
  block
    Prefix
    Suffix

def Prefix = PNGContents
def Suffix = PDFContents
