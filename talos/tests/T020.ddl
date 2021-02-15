-- Zoo4 from notes.org. Function call argument entanglement

def P a b = {
    a < b is true;
}

def Main = {
    x = Match1 ('a'..'z');
    y = Match1 ('a'..'z');
    P x y;
}
