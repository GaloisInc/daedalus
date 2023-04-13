import common

def Main =
  block
    BytesThenSuffix
    END

def BytesThenSuffix =
  First
    Token (Match "SUFFIX")
    block
      UInt8
      BytesThenSuffix
