-- Test order of choose
-- or not that useful test

def CA = { Match1 'a' ; ^ "a" }

def CB = { Match1 'a' ; ^ "b" }

def Main = {
    a = Match1 'a';
    t = Choose {
      x = CA ;
      y = CB
    };
    END
}
