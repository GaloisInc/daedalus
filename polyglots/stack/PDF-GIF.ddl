 {-|
  Name: PDF-GIF
  Description: This file contains a Daedalus description of a PDF/GIF
  polyglot that composes the files generated from PDF.ddl and GIF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import GIF

def Main =
  block
    Prefix
    Suffix

def Prefix = GIFContents
def Suffix = PDFContents
