 {-|
  Name: PDF-EXE
  Description: This file contains a Daedalus description of a PDF/EXE
  polyglot that composes the files generated from PDF.ddl and EXE.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import EXE

def Main =
  block
    Prefix
    Suffix

def Prefix = EXEContents
def Suffix = PDFContents
