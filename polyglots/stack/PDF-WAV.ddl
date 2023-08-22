 {-|
  Name: PDF-WAV
  Description: This file contains a Daedalus description of a PDF/WAV
  polyglot that composes the files generated from PDF.ddl and WAV.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import WAV

def Main =
  block
    Prefix
    Suffix

def Prefix = WAVContents
def Suffix = PDFContents
