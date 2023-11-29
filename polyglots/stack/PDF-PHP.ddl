 {-|
  Name: PDF-PHP
  Description: This file contains a Daedalus description of a PDF/PHP
  polyglot that composes the files generated from PDF.ddl and PHP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import PHP

def Main =
  block
    Prefix
    Suffix

def Prefix = PHPContents
def Suffix = PDFContents
