 {-|
  Name: PDF-WEBP
  Description: This file contains a Daedalus description of a PDF/WEBP
  polyglot that composes the files generated from PDF.ddl and WEBP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import WEBP

def Main =
  block
    Prefix
    Suffix

def Prefix = WEBPContents
def Suffix = PDFContents
