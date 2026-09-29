def Node =
  block
    value = UInt8
    next = Optional Node

def Value =
  First
    done = block
      Match [0]
      UInt8
    more = Pair

def Pair =
  block
    Match [1]
    value = UInt8
    next = Value

def Main =
  block
    SetStream (arrayStream [1, 2, 3])
    node = Node

    SetStream (arrayStream [1, 10, 1, 20, 0, 30])
    value = Value
