 {-|
  Name: PDF-ASM
  Description: This file contains a Daedalus description of a PDF/ASM
  polyglot that composes the files generated from PDF.ddl and ASM.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import ASM

def Main =
  block
    Prefix
    Suffix

def Prefix = ASMContents
def Suffix = PDFContents
