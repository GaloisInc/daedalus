-- Zoo4 from notes.org. Function call result entanglement

def P (a : uint 64) (c: uint 64) = {
    @b = UInt8 as uint 64;
    c > 7 is true; 
    ^ a + b; 
}

def Main = {
    x = UInt8 as uint 64;
    z = P x x; 
    z < 10 is true;
}
