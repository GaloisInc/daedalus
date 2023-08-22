 {-|
  Name: PDF-ICO
  Description: This file contains a Daedalus description of a PDF/ICO
  polyglot that composes the files generated from PDF.ddl and ICO.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import ICO

def Main =
  block
    Prefix
    Suffix

def Prefix = ICOContents
def Suffix = PDFContents
