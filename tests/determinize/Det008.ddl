-- testing Or at depth greater than zero

def Main = { T1; T2; T3; END }

def T1 = {$['a'] ; ($['a'] <| $['b']) } <| { $['a'] ; $['c'] }
def T2 = {$['a'] ; ($['a'] <| $['b']) } <| { $['a'] ; $['b'] }
def T3 = {$['a'] ; ($['a'] <| $['b']) }  | { $['a'] ; $['a'] }
