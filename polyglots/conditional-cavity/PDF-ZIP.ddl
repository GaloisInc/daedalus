{-|
  Name: PDF-ZIP

  Description: This file contains a Daedalus description of a polyglot built
  from a PDF file wrapping a ZIP in a multi-line comment.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import PDF
import ZIP

def Main =
  block
    -- Specification of FMT1.
    -- Requirements: No end-of-file delimiter.
    FMT1Content

    -- Start-of-cavity delimiter.
    FMT1CavityStartDelimiter

    -- Specification of FMT2.
    -- Requirements: Cannot contain FMT1CavityEndDelimiter.
    FMT2Content

    -- End-of-cavity delimiter.
    FMT1CavityEndDelimiter

def FMT1Content = PDFContent

def FMT1CavityStartDelimiter = PDFCavityStartDelimiter

def FMT1CavityEndDelimiter = PDFCavityEndDelimiter

def FMT2Content = ZIPContent
