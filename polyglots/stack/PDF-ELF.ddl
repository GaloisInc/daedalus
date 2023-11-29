 {-|
  Name: PDF-ELF
  Description: This file contains a Daedalus description of a PDF/ELF
  polyglot that composes the files generated from PDF.ddl and ELF.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import ELF

def Main =
  block
    Prefix
    Suffix

def Prefix = ELFContents
def Suffix = PDFContents
