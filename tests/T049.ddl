
def V =
  First
    num  = UInt8
    null = Accept


def TD =
  block
    obj = TDDef

def TDDef =
  First
    stream = ResolveRef 0
    value  = V

def ResolveDeclRef (x : int) : TD

def ResolveRef (x : int) : V =
  block
    let p = ResolveDeclRef x
    p.obj is value


