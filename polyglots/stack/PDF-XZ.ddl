 {-|
  Name: PDF-XZ
  Description: This file contains a Daedalus description of a PDF/XZ
  polyglot that composes the files generated from PDF.ddl and XZ.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import XZ

def Main =
  block
    Prefix
    Suffix

def Prefix = XZContents
def Suffix = PDFContents
