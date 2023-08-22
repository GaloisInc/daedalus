 {-|
  Name: PDF-CPIO
  Description: This file contains a Daedalus description of a PDF/CPIO
  polyglot that composes the files generated from PDF.ddl and CPIO.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import CPIO

def Main =
  block
    Prefix
    Suffix

def Prefix = CPIOContents
def Suffix = PDFContents
