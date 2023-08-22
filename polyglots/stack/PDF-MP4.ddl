 {-|
  Name: PDF-MP4
  Description: This file contains a Daedalus description of a PDF/MP4
  polyglot that composes the files generated from PDF.ddl and MP4.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import MP4

def Main =
  block
    Prefix
    Suffix

def Prefix = MP4Contents
def Suffix = PDFContents
