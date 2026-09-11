def Main : uint 8 = Outer

def Outer : uint 8 = 1 + Middle

def Middle : uint 8 = Inner

def Inner : uint 8 = 1 + 255
