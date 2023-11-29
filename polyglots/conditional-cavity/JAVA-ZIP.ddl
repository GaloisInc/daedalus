{-|
  Name: JAVA-ZIP

  Description: This file contains a Daedalus description of a polyglot built
  from a JAVA file wrapping a ZIP in a multi-line comment.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

import JAVA
import ZIP

def Main =
  block
    -- Specification of FMT1.
    -- Requirements: No end-of-file delimiter.
    FMT1Content

    -- Start-of-cavity delimiter.
    FMT1CavityStartDelimiter

    -- Specification of FMT2.
    -- Requirements:
    --  - Cannot contain FMT1CavityEndDelimiter.
    --  - Bytes must be within a character encoding accepted by javac, e.g.
    --    ISO8859_1, see
    --    https://docs.oracle.com/javase/8/docs/technotes/guides/intl/encoding.doc.html
    FMT2Content

    -- End-of-cavity delimiter.
    FMT1CavityEndDelimiter

def FMT1Content = JAVAContent

def FMT1CavityStartDelimiter = JAVACavityStartDelimiter

def FMT1CavityEndDelimiter = JAVACavityEndDelimiter

def FMT2Content = ZIPContent
