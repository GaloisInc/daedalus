 {-|
  Name: PDF-MOV
  Description: This file contains a Daedalus description of a PDF/MOV
  polyglot that composes the files generated from PDF.ddl and MOV.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import MOV

def Main =
  block
    Prefix
    Suffix

def Prefix = MOVContents
def Suffix = PDFContents
