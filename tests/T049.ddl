
def V = Choose {
  null = {};
  num  = UInt8;
}


def TD = {
  obj = TDDef;
}

def TDDef = Choose {
  stream = ResolveRef 0;
  value = V;
}

def ResolveDeclRef (x : int) : TD

def ResolveRef (x : int) : V = {
  @p = ResolveDeclRef x;
  p.obj is value;
}


