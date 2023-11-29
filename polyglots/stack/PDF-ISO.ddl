 {-|
  Name: PDF-ISO
  Description: This file contains a Daedalus description of a PDF/ISO
  polyglot that composes the files generated from PDF.ddl and ISO.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import ISO

def Main =
  block
    Prefix
    Suffix

def Prefix = ISOContents
def Suffix = PDFContents
