 {-|
  Name: PDF-AVI
  Description: This file contains a Daedalus description of a PDF/AVI
  polyglot that composes the files generated from PDF.ddl and AVI.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import AVI

def Main =
  block
    Prefix
    Suffix

def Prefix = AVIContents
def Suffix = PDFContents
