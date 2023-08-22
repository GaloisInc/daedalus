 {-|
  Name: PDF-SWF
  Description: This file contains a Daedalus description of a PDF/SWF
  polyglot that composes the files generated from PDF.ddl and SWF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import SWF

def Main =
  block
    Prefix
    Suffix

def Prefix = SWFContents
def Suffix = PDFContents
