 {-|
  Name: PDF-MP3
  Description: This file contains a Daedalus description of a PDF/MP3
  polyglot that composes the files generated from PDF.ddl and MP3.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import MP3

def Main =
  block
    Prefix
    Suffix

def Prefix = MP3Contents
def Suffix = PDFContents
