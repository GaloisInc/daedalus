
def Foo (x : maybe int) = ^ x
def Main = { a = just 1; b = Foo a }
