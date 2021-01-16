{- Check the well bracketing of bias vs unbias choice -}


def A = Choose1 {a = Many (Match1 'a'); b = Many (Match1 'a') }

def B = Many (Match1 'a')

def Main = {x = A}
