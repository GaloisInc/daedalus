{- Check the call -}

def Inner =
  Choose
  { x = Match ""
  ; y = { @u = Match1 'a'; @v = A; @w = Match1 'b'}
  ; z = { @u = Match1 'c'; @v = A; @w = Match1 'd'}
  }

def A = {@op = Match1 '<'; @m = Inner; @cl = Match1 '>'}

def Main = {x = A; END}
