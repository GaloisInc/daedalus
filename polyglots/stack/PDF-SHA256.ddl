 {-|
  Name: PDF-SHA256
  Description: This file contains a Daedalus description of a PDF/SHA256
  polyglot that composes the files generated from PDF.ddl and SHA256.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import SHA256

def Main =
  block
    Prefix
    Suffix

def Prefix = SHA256Contents
def Suffix = PDFContents
