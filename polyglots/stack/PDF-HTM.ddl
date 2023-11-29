 {-|
  Name: PDF-HTM
  Description: This file contains a Daedalus description of a PDF/HTM
  polyglot that composes the files generated from PDF.ddl and HTM.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import HTM

def Main =
  block
    Prefix
    Suffix

def Prefix = HTMContents
def Suffix = PDFContents
