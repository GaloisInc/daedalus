import common

def Main = 
  block
    Many Block
    END

def Block =
  block
    TokenM "LENGTH"
    let len = Token Natural
    Many len UInt8
