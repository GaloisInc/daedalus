 {-|
  Name: PDF-ZIP
  Description: This file contains a Daedalus description of a PDF/ZIP
  polyglot that composes the files generated from PDF.ddl and ZIP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import ZIP

def Main =
  block
    Prefix
    Suffix

def Prefix = ZIPContents
def Suffix = PDFContents
