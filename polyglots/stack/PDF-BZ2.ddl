 {-|
  Name: PDF-BZ2
  Description: This file contains a Daedalus description of a PDF/BZ2
  polyglot that composes the files generated from PDF.ddl and BZ2.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import BZ2

def Main =
  block
    Prefix
    Suffix

def Prefix = BZ2Contents
def Suffix = PDFContents
