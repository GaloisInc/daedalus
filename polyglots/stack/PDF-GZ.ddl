 {-|
  Name: PDF-GZ
  Description: This file contains a Daedalus description of a PDF/GZ
  polyglot that composes the files generated from PDF.ddl and GZ.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import GZ

def Main =
  block
    Prefix
    Suffix

def Prefix = GZContents
def Suffix = PDFContents
