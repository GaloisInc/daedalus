 {-|
  Name: PDF-TAR
  Description: This file contains a Daedalus description of a PDF/TAR
  polyglot that composes the files generated from PDF.ddl and TAR.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import TAR

def Main =
  block
    Prefix
    Suffix

def Prefix = TARContents
def Suffix = PDFContents
