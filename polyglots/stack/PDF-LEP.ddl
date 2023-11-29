 {-|
  Name: PDF-LEP
  Description: This file contains a Daedalus description of a PDF/LEP
  polyglot that composes the files generated from PDF.ddl and LEP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import LEP

def Main =
  block
    Prefix
    Suffix

def Prefix = LEPContents
def Suffix = PDFContents
