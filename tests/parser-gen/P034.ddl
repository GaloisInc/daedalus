-- Test order of choose
-- or not that useful test

def CA = { $['a'] ; ^ "a" }

def CB = { $['a'] ; ^ "b" }

def Main = {
    a = $['a'];
    t = Choose {
      x = CA ;
      y = CB
    };
    END
}
