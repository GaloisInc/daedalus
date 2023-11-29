 {-|
  Name: PDF-FLAC
  Description: This file contains a Daedalus description of a PDF/FLAC
  polyglot that composes the files generated from PDF.ddl and FLAC.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import FLAC

def Main =
  block
    Prefix
    Suffix

def Prefix = FLACContents
def Suffix = PDFContents
