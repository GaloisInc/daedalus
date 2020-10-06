module Daedalus.VM.FreeVars where

import Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.VM

defines :: Instr -> [BV]
defines instr =
  case instr of
    SetInput {}     -> []
    Say {}          -> []
    Output {}       -> []
    Notify {}       -> []
    CallPrim v _ _  -> [v]
    GetInput v      -> [v]
    Spawn v _       -> [v]
    NoteFail        -> []
    Free {}         -> []
    Let v _         -> [v]

defineSet :: Instr -> Set BV
defineSet = Set.fromList . defines

freeVars :: FreeVars t => t -> [VMVar]
freeVars t = freeVars' t []

freeVarSet :: FreeVars t => t -> Set VMVar
freeVarSet = Set.fromList . freeVars



class FreeVars t where
  -- | We return a list rather than a set so that we can notice
  -- multiple uses of the same variable
  freeVars' :: t -> [VMVar] -> [VMVar]

instance FreeVars t => FreeVars [t] where
  freeVars' xs = foldr (\x rest -> freeVars' x . rest) id xs

instance (FreeVars a, FreeVars b) => FreeVars (a,b) where
  freeVars' (a,b) = freeVars' a . freeVars' b

instance FreeVars E where
  freeVars' expr =
    case expr of
      EUnit         -> id
      ENum {}       -> id
      EBool {}      -> id
      EMapEmpty {}  -> id
      ENothing {}   -> id

      EBlockArg ba  -> (ArgVar ba :)
      EVar      bv  -> (LocalVar bv :)

instance FreeVars JumpPoint where
  freeVars' (JumpPoint _ es) = freeVars' es

instance FreeVars CInstr where
  freeVars' cinstr =
    case cinstr of
      Jump l            -> freeVars' l
      JumpIf e l1 l2    -> freeVars' (e, (l1,l2))
      Yield             -> id
      ReturnNo          -> id
      ReturnYes e       -> freeVars' e
      Call _ _ l1 l2 es -> freeVars' (es,(l1,l2))
      TailCall _ _ es   -> freeVars' es
      ReturnPure e      -> freeVars' e

instance FreeVars Instr where
  freeVars' instr =
    case instr of
      SetInput e      -> freeVars' e
      Say {}          -> id
      Output e        -> freeVars' e
      Notify e        -> freeVars' e
      CallPrim _ _ es -> freeVars' es
      GetInput _      -> id
      Spawn _ l       -> freeVars' l
      NoteFail        -> id
      Free xs         -> (Set.toList xs ++)
      Let _ e         -> freeVars' e

