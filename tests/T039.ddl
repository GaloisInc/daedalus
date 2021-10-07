
def Main =
  block
    Block 5 (Match "hi")
    Match "there"

def Block n P =
  block
    let s = GetStream
    SetStream (Take n s)
    $$    = P
    SetStream (Drop n s)


