-- Zoo4 from notes.org. Function call argument entanglement

def P a b = {
    a < b is true;
}

def Main = {
    x = $['a'..'z'];
    y = $['a'..'z'];
    P x y;
}
