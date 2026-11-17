-- Regression test: specialization substitutes byte-class parameters.
def Parameterized $c = { $c; $c; $c }

def $n = '0' .. '9'

def Main =
  block
    SetStream (arrayStream "123")
    Parameterized $n
