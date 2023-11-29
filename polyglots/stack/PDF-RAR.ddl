 {-|
  Name: PDF-RAR
  Description: This file contains a Daedalus description of a PDF/RAR
  polyglot that composes the files generated from PDF.ddl and RAR.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import RAR

def Main =
  block
    Prefix
    Suffix

def Prefix = RARContents
def Suffix = PDFContents
