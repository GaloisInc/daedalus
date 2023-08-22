 {-|
  Name: PDF-JPG
  Description: This file contains a Daedalus description of a PDF/JPG
  polyglot that composes the files generated from PDF.ddl and JPG.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import JPG

def Main =
  block
    Prefix
    Suffix

def Prefix = JPGContents
def Suffix = PDFContents
