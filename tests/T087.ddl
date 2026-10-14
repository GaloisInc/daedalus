-- A structure containing a float is not a valid map key.
def FloatKey = { tag = 1 : uint 8, value = 1 : float }

def Main = insert FloatKey 10 empty
