
def a = for (s = 0 : int; i in rangeUp 5) s + i
def A = for (s = 0 : int; i in rangeUp 5) ^ s + i

def Main =
  block
    a1 = ^ a
    a2 = A

