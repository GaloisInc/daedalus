def Digit   = { @d = '0' .. '9'; ^ d - '0' as int }
def Natural = { ds = Many (1..) Digit}
def Main    = { x = Natural; END}
