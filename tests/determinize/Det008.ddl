-- testing Or at depth greater than zero

def Main = { T1; T2; T3; END }

def T1 = {Match1 'a' ; (Match1  'a' <| Match1 'b') } <| {Match1 'a' ; Match1 'c'}
def T2 = {Match1 'a' ; (Match1  'a' <| Match1 'b') } <| {Match1 'a' ; Match1 'b'}
def T3 = {Match1 'a' ; (Match1  'a' <| Match1 'b') }  | {Match1 'a' ; Match1 'a'}
