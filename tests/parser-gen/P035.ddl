-- map with union selection

def A = {
    a = Match "a";
    b = Match "b";
}

def RepA = { $$ = Many A }

def F x a b =
  { Match x
  ; @v = Match b
  ; @w = Match a
  ; ^ "BA"
  }

def Main = {
    @t = RepA;
    Match "S";
    m = for (r = ""; x in t) (F " F" x.a x.b);
    n = map (x in t) (F " M" x.a x.b);
    Match " S";
    Match "ba";
    END
}
