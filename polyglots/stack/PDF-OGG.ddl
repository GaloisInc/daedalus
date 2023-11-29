 {-|
  Name: PDF-OGG
  Description: This file contains a Daedalus description of a PDF/OGG
  polyglot that composes the files generated from PDF.ddl and OGG.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import OGG

def Main =
  block
    Prefix
    Suffix

def Prefix = OGGContents
def Suffix = PDFContents
