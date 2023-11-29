 {-|
  Name: PDF-RTF
  Description: This file contains a Daedalus description of a PDF/RTF
  polyglot that composes the files generated from PDF.ddl and RTF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import RTF

def Main =
  block
    Prefix
    Suffix

def Prefix = RTFContents
def Suffix = PDFContents
