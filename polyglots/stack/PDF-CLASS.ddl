 {-|
  Name: PDF-CLASS
  Description: This file contains a Daedalus description of a PDF/CLASS
  polyglot that composes the files generated from PDF.ddl and CLASS.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import CLASS

def Main =
  block
    Prefix
    Suffix

def Prefix = CLASSContents
def Suffix = PDFContents
