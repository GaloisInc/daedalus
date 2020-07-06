module Value where

import AST

data V  = VChar Char
        | VBool Bool
        | VInt Int
        | VInput I
        | VUnit
          deriving (Show,Eq)

data I = I { loc :: Int, bytes :: String }
  deriving (Eq,Show)

type FunV = [V] -> V

primFuns :: [(F,[V] -> V)]
primFuns =
  [ ("true",TBool)    |-> \[]           -> VBool True
  , ("false",TBool)   |-> \[]           -> VBool False
  , ("unit",TUnit)    |-> \[]           -> VUnit
  , ("null",TBool)    |-> \[VInput xs]  -> VBool (null (bytes xs))
  , ("head",TChar)    |-> \[VInput xs]  -> VChar (head (bytes xs))
  , ("tail",TInput)   |-> \[VInput xs]  -> VInput I { loc = loc xs + 1
                                                    , bytes = tail (bytes xs)
                                                    }
  , ("eq",TBool)      |-> \[x,y]        -> VBool (x == y)
  , ("add",TInt)      |-> \[x,y]        -> case (x,y) of
                                             (VInt a, VInt b) -> VInt (a+b)
                                             _ -> error "add: type error"
  ]
  where x |-> y = (x,y)


