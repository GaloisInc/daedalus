 {-|
  Name: PDF-WAD
  Description: This file contains a Daedalus description of a PDF/WAD
  polyglot that composes the files generated from PDF.ddl and WAD.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import WAD

def Main =
  block
    Prefix
    Suffix

def Prefix = WADContents
def Suffix = PDFContents
