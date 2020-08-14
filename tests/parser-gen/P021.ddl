def A = Choose1
    { x = Match "a"
    ; y = Match "a"
    }

def B = Choose
    { z = Match "a"
    ; t = Match "a"
    }

def Main = { p = B; x = Many A; END }
