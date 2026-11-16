def Ints =
  union
    u8: uint 8

def Main : [Ints] =
  block
    let b0 = builder
    let b1 = emit b0 {| u8 = (1 : uint 8) |}
    let s = { b = b1 }
    let b2 = emit s.b {| u8 = (1 : uint 8) |}
    build b2

def Inner =
  struct
    x: uint 8

def MainUnion : Inner =
  {| A = { x = (1 : uint 8) } |} is A
