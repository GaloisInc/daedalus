 {-|
  Name: ZIP-PDF
  Description: This file contains a Daedalus description of a ZIP/PDF
  polyglot that composes the files generated from ZIP.ddl and PDF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import PDF

def Main =
  block
    Prefix
    Suffix

def Prefix = PDFContents
def Suffix = ZIPContents
