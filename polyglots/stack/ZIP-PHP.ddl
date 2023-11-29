 {-|
  Name: ZIP-PHP
  Description: This file contains a Daedalus description of a ZIP/PHP
  polyglot that composes the files generated from ZIP.ddl and PHP.ddl.
  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import ZIP
import PHP

def Main =
  block
    Prefix
    Suffix

def Prefix = PHPContents
def Suffix = ZIPContents
