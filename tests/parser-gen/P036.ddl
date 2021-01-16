-- testing WithSem annotations on Map operations


def A = Choose { x = 'a'; y = 'b' }

--def Insert2 k v m = Insert k v m

-- cannot insert twice
def Case1 =
  { @u = Insert "x" 'a' empty
  ; @v = Insert "x" 'b' u
  ; ^ v
  }

-- lookup fail
def Case2 =
  { @u = Insert "x" 'a' empty
  ; @v = Lookup "y" u
  ; ^ v
  }

-- If insert succeeds with NoSem
def Case3 =
  { @u = @Insert "y" 'a' empty
  ; ^ u
  }

-- If lookup succeeds with NoSem
def Case4 =
  { @u = Insert "x" 'a' empty
  ; @v = @Lookup "x" u
  ; v == {} is true
  ; ^ u
  }

def Main =
  { x = Match1 ('0'..'9')
  ; y = Choose1
      { c1 = { x == '1' is true
        ; Case1
        }
      ; c2 = { x == '2' is true
        ; Case2
        }
      ; c3 = { x == '3' is true
        ; Case3
        }
      ; c4 = { x == '4' is true
        ; Case4
        }
      }
  }