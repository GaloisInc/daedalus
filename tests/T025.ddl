-- Test maps

def Main = { @k = UInt8
           ; @v = UInt8
           ; r = Insert k v empty
           ; vr = Lookup 'x' r
           }
