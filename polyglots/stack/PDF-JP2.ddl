 {-|
  Name: PDF-JP2
  Description: This file contains a Daedalus description of a PDF/JP2
  polyglot that composes the files generated from PDF.ddl and JP2.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import JP2

def Main =
  block
    Prefix
    Suffix

def Prefix = JP2Contents
def Suffix = PDFContents
