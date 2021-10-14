import Daedalus

-- ENTRY
def Main =
  block
    size                = BE32
    preferred_cmm_type  = Many 4 UInt8
    version             = VersionField

def VersionField =
  block
    major        = UInt8
    let min_bf   = UInt8
    minor        = min_bf >> 4 as! uint 4
    bugfix       = min_bf      as! uint 4
    Match [0x00, 0x00]




