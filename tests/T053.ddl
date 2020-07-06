def Main = {

  -- exceptions cancel all alternatives
  ex1 = Check { Throw <| Ok 1 };      -- nothing
  ex2 = Check { Ok 1  <| Throw };     -- just 1
  ex3 = Check { Throw  | Ok 1 };      -- nothing
  ex4 = Check { Ok 1   | Throw };     -- nothing

  -- `try` converts exceptions to ordinary errors
  ex5 = Check { try Throw <| Ok 1 };  -- just 1
  ex6 = Check { try Throw  | Ok 1 };  -- just 1

  -- a typical use case
  ex7 = Check { { Ok 1; commit; PFail; Ok 2 }        -- nothing
             <| Ok 3
              };

  -- `commit` only affects errors, so this is still OK
  ex8 = Check {
          @x = { commit; Ok 1 } | Ok 2;
          x == 2;
        };

}


-- A parser that always succesd
def Ok (x : int) = ^ x

-- A parser that always fails
def PFail = Choose {} : int

-- Errors after `commit` are turned into exceptions.
def Throw = { commit; PFail }

-- Turn a parser that might abort, into one that always succeeds with maybe.
-- `try` catches exceptions and turns them into parse errors
def Check P = Optional (try P)







