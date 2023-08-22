 {-|
  Name: PDF-FLV
  Description: This file contains a Daedalus description of a PDF/FLV
  polyglot that composes the files generated from PDF.ddl and FLV.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import FLV

def Main =
  block
    Prefix
    Suffix

def Prefix = FLVContents
def Suffix = PDFContents
