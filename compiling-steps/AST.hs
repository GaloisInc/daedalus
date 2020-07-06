module AST where

import Data.Map(Map)
import qualified Data.Map as Map

type N  = (String,T)      -- local names: both binders and uses have types
type F  = (String,T)      -- top-level names: types is the result

data T  = TChar
        | TInt
        | TBool
        | TUnit
        | TInput
          deriving (Show,Eq,Ord)

data P  = Do N P P
        | P :|| P
        | P :<| P
        | If E P P
        | Call F [E]
        | Pure E
        | Fail T
        | Peek
        | SetInput E
          deriving Show

data E  = Var N
        | Char Char
        | Int Int
        | App F [E]
          deriving Show


data Fun = Fun F [N] P


--------------------------------------------------------------------------------
typeOf :: P -> T
typeOf pa =
  case pa of
    Do _ _ q   -> typeOf q
    p :|| _    -> typeOf p
    p :<| _    -> typeOf p
    If _ p _   -> typeOf p
    Call f _   -> snd f
    Pure e     -> typeOfE e
    Fail t     -> t
    Peek       -> TInput
    SetInput _ -> TUnit

typeOfE :: E -> T
typeOfE expr =
  case expr of
    Var n   -> snd n
    Char _  -> TChar
    Int _   -> TInt
    App f _ -> snd f



--------------------------------------------------------------------------------

data Effect = OnlyNo | OnlyYes | NoYes
  deriving (Eq,Ord,Show)

instance Semigroup Effect where
  x <> y = case (x,y) of
             (OnlyNo,  OnlyNo)  -> OnlyNo
             (OnlyYes, OnlyYes) -> OnlyYes
             _                  -> NoYes

effect :: Map F Effect -> P -> Effect
effect funs = go
  where
  go pa =
    case pa of
      Do _ p q    -> case go p of
                       OnlyYes -> go q
                       OnlyNo  -> OnlyNo
                       NoYes   -> case go q of
                                    OnlyNo -> OnlyNo
                                    _      -> NoYes
      p :|| q     -> case go p of
                       OnlyYes -> OnlyYes
                       OnlyNo  -> go q
                       NoYes   -> case go q of
                                    OnlyYes -> OnlyYes
                                    _ -> NoYes
      p :<| q     -> case go p of
                      OnlyYes -> OnlyYes
                      OnlyNo  -> go q
                      NoYes   -> case go q of
                                   OnlyYes -> OnlyYes
                                   _       -> NoYes
      If _ p q    -> go p <> go q
      Call f _    -> case Map.lookup f funs of
                       Nothing -> NoYes
                       Just e  -> e
      Pure _      -> OnlyYes
      Fail _      -> OnlyNo
      Peek        -> OnlyYes
      SetInput _  -> OnlyYes

funEffect :: [Fun] -> Map F Effect
funEffect fs = fst $ head $ dropWhile (uncurry (/=)) $ zip approx $ tail approx
  where
  effFun es (Fun f _ e) = (f, effect es e)
  step es               = Map.fromList (map (effFun es) fs)
  approx                = iterate step Map.empty


--------------------------------------------------------------------------------
fTrue, fFalse, fUnit :: E
fTrue   = App ("true",TBool)   []
fFalse  = App ("false",TBool)  []
fUnit   = App ("unit",TUnit)   []

fHead, fTail, fNull :: E -> E
fHead e = App ("head",TChar)  [e]
fTail e = App ("tail",TInput) [e]
fNull e = App ("null",TBool)  [e]

(===) :: E -> E -> E
x === y = App ("eq",TBool) [x,y]

fAdd :: E -> E -> E
fAdd x y = App ("add",TInt) [x,y]


--------------------------------------------------------------------------------

(>>>) :: P -> P -> P
p >>> q = Do ("_",TUnit) p q

pGetChar :: P
pGetChar =
  Do i Peek
   $ If (fNull (Var i))
        (Fail TChar)
        (SetInput (fTail (Var i)) >>> Pure (fHead (Var i)))
  where i = ("i",TInput)

pMatch :: E -> P
pMatch e =
  let x = ("x",TChar)
  in
  Do x pGetChar
   $ If (Var x === e)
        (Pure (Var x))
        (Fail TChar)

--------------------------------------------------------------------------------


fun_P      = ("P",TChar)
def_P      = Fun fun_P [] (pMatch (Char 'A'))

fun_Q      = ("Q",TChar)
def_Q      = Fun fun_Q [] (pMatch (Char 'A') :|| pMatch (Char 'B'))

fun_S     = ("S",TChar)
def_S     = Fun fun_S []
          $ Do x pOr
          $ If (Var x === Int 2)
               (Pure (Char 'Y'))
               (Fail TChar)
  where
  x   = ("x",TInt)
  pOr = (pMatch (Char 'A') >>> Pure (Int 1))
    :|| (pMatch (Char 'A') >>> Pure (Int 2))



fun_loop = ("Loop",TInt)
def_loop = Fun fun_loop [x] (lhs :<| Pure (Var x))
  where
  x   = ("x",TInt)
  lhs = Do ("_", TChar) (Call fun_Q [])
         $               Call fun_loop [ fAdd (Var x) (Int 1) ]




ex0 :: ([ Fun ], P)
ex0 = ([], pMatch (Char 'A'))

ex1 :: ([ Fun ], P)
ex1 = ( [ def_Q, def_loop ]
      , Call fun_loop [Int 0]
      )

ex2 :: ([ Fun ], P)
ex2 = ( [ def_S ]
      , Call fun_S []
      )


