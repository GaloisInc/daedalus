 {-|
  Name: PDF-Y4M
  Description: This file contains a Daedalus description of a PDF/Y4M
  polyglot that composes the files generated from PDF.ddl and Y4M.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import Y4M

def Main =
  block
    Prefix
    Suffix

def Prefix = Y4MContents
def Suffix = PDFContents
