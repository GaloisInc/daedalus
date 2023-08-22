 {-|
  Name: PDF-WEBM
  Description: This file contains a Daedalus description of a PDF/WEBM
  polyglot that composes the files generated from PDF.ddl and WEBM.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import WEBM

def Main =
  block
    Prefix
    Suffix

def Prefix = WEBMContents
def Suffix = PDFContents
