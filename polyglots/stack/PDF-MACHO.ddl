 {-|
  Name: PDF-MACHO
  Description: This file contains a Daedalus description of a PDF/MACHO
  polyglot that composes the files generated from PDF.ddl and MACHO.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import MACHO

def Main =
  block
    Prefix
    Suffix

def Prefix = MACHOContents
def Suffix = PDFContents
