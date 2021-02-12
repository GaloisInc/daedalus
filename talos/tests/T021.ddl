-- Zoo4 from notes.org. Function call result entanglement

def P a = {
    @b = UInt8;
    ^ a + b; 
}

def Main = {
    x = UInt8;
    z = P x; 
    z < 10 is true;
}
