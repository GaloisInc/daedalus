
bitdata myBD2 where
  B = 0x42
  C = 0x43

bitdata myBD3 where
  D = { 0x41 ; x : myBD2 }
  E = { 0x42 ; x : myBD2 }

def Main =
  block
    SetStream (arrayStream ['A', 'B'])
    @v = UInt8 # UInt8;
    v as? myBD3
    