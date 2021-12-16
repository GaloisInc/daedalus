import Daedalus

def BTreeHeader =
  block
    off    = BEUInt64
    count  = BEUInt64

def BTree  =
  block
    count = BEUInt64
    hdr   = Many count BTreeHeader
    s     = GetStream
    map (h in hdr)
      block
        SetStreamAt h.off s
        Many h.count BTree
