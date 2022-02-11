bitdata B where C = { x : uint 8, y : uint 8 }

def Main = ((0xFFFF : uint 16) as B) as uint 16
