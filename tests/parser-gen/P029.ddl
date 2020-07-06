{- Check the call -}

def Inner =
  Choose
  { x = ""
  ; y = { @u = 'a'; @v = A; @w = 'b'}
  ; z = { @u = 'c'; @v = A; @w = 'd'}
  }

def A = {@op = '<'; @m = Inner; @cl = '>'}

def Main = {x = A; END}
