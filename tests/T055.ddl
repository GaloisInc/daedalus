def Main = Default 'a' (Match1 'b')
def Default a P = P <| ^ a
