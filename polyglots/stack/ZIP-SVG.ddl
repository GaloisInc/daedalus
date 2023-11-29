 {-|
  Name: ZIP-SVG
  Description: This file contains a Daedalus description of a ZIP/SVG
  polyglot that composes the files generated from ZIP.ddl and SVG.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import SVG

def Main =
  block
    Prefix
    Suffix

def Prefix = SVGContents
def Suffix = ZIPContents
