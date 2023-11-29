 {-|
  Name: PDF-SVG
  Description: This file contains a Daedalus description of a PDF/SVG
  polyglot that composes the files generated from PDF.ddl and SVG.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import SVG

def Main =
  block
    Prefix
    Suffix

def Prefix = SVGContents
def Suffix = PDFContents
