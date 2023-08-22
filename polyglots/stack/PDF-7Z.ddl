 {-|
  Name: PDF-7Z
  Description: This file contains a Daedalus description of a PDF/7Z
  polyglot that composes the files generated from PDF.ddl and 7Z.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import 7Z

def Main =
  block
    Prefix
    Suffix

def Prefix = 7ZContents
def Suffix = PDFContents
