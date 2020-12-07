def BE16 = UInt8 # UInt8
def BE32 = BE16 # BE16

def Main = {
  SetStream (arrayStream [1,2,3,4]);
  BE32;
}
