def A = Choose1
    { x = "a"
    ; y = "a"
    }

def B = Choose
    { z = "a"
    ; t = "a"
    }

def Main = { p = B; x = Many A; END }
