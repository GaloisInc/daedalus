-- map with union selection

def A = {
    a = "a";
    b = "b";
}

def RepA = { $$ = Many A }

def F x a b =
  { x
  ; @v = b
  ; @w = a
  ; ^ "BA"
  }

def Main = {
    @t = RepA;
    "S";
    m = for (r = ""; x in t) (F " F" x.a x.b);
    n = map (x in t) (F " M" x.a x.b);
    " S";
    "ba";
    END
}