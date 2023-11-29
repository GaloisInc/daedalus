 {-|
  Name: PDF-MKV
  Description: This file contains a Daedalus description of a PDF/MKV
  polyglot that composes the files generated from PDF.ddl and MKV.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import MKV

def Main =
  block
    Prefix
    Suffix

def Prefix = MKVContents
def Suffix = PDFContents
