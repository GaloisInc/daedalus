module Daedalus.VM.FreeVars where

import Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.VM

defines :: Instr -> Set BV
defines instr =
  case instr of
    SetInput {}     -> Set.empty
    Say {}          -> Set.empty
    Output {}       -> Set.empty
    Notify {}       -> Set.empty
    CallPrim _ _ v  -> Set.singleton v
    GetInput v      -> Set.singleton v
    Spawn _ v       -> Set.singleton v
    NoteFail        -> Set.empty
    Free {}         -> Set.empty

class FreeVars t where
  freeVars :: t -> Set VMVar

instance FreeVars t => FreeVars [t] where
  freeVars = Set.unions . map freeVars

instance (FreeVars a, FreeVars b) => FreeVars (a,b) where
  freeVars (a,b) = freeVars a `Set.union` freeVars b

instance FreeVars E where
  freeVars expr =
    case expr of
      EUnit         -> Set.empty
      ENum {}       -> Set.empty
      EBool {}      -> Set.empty
      EByteArray {} -> Set.empty
      EMapEmpty {}  -> Set.empty
      ENothing {}   -> Set.empty

      EBlockArg ba  -> Set.singleton (ArgVar ba)
      EVar      bv  -> Set.singleton (LocalVar bv)

instance FreeVars JumpPoint where
  freeVars (JumpPoint _ es) = freeVars es

instance FreeVars CInstr where
  freeVars cinstr =
    case cinstr of
      Jump l            -> freeVars l
      JumpIf e l1 l2    -> freeVars (e, (l1,l2))
      Yield             -> Set.empty
      ReturnNo          -> Set.empty
      ReturnYes e       -> freeVars e
      Call _ _ l1 l2 es -> freeVars (es,(l1,l2))
      TailCall _ _ es   -> freeVars es
      ReturnPure e      -> freeVars e

instance FreeVars Instr where
  freeVars instr =
    case instr of
      SetInput e      -> freeVars e
      Say {}          -> Set.empty
      Output e        -> freeVars e
      Notify e        -> freeVars e
      CallPrim _ es _ -> freeVars es
      GetInput _      -> Set.empty
      Spawn l _       -> freeVars l
      NoteFail        -> Set.empty
      Free x          -> Set.singleton x


