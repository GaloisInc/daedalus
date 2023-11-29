{-|
  Name: conditional-cavity-shape

  Description: This file contains a Daedalus description of a polyglot built
  from a data format FMT1 with a conditional internal cavity (e.g. a comment
  string) composed with a format FMT2 with both prefix and suffix cavities.

  Maintainer     : Cole Schlesinger <coles@galois.com>
  Stability      : provisional
-}

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

-- def FMT1Content =

-- def FMT1CavityStartDelimiter = 

-- def FMT1CavityEndDelimiter = 

-- def FMT2Content =
