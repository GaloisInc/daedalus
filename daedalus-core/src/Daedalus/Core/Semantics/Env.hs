{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}

module Daedalus.Core.Semantics.Env where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Word(Word8)

import RTS.ParserTraced as RTS
import Daedalus.Value

import Daedalus.Panic(panic)
import Daedalus.PP (pp, hsep, punctuate)
import Daedalus.Core(Name,FName,TName,TDecl)

data Env = Env
  { vEnv    :: Map Name Value
  , cEnv    :: Map FName Value   -- ^ Top level constants
  , fEnv    :: Map FName ([Value] -> Value)
  , bEnv    :: Map FName ([Value] -> Word8 -> Bool)
  , gEnv    :: Map FName ([Value] -> Parser Value)
  , tEnv    :: Map TName TDecl
  }

emptyEnv :: Env
emptyEnv =
  Env { vEnv = mempty, cEnv = mempty, fEnv = mempty, bEnv = mempty
      , gEnv = mempty, tEnv = mempty }

lookupVar :: Name -> Env -> Value
lookupVar x env =
  case Map.lookup x (vEnv env) of
    Just v  -> v
    Nothing -> panic "lookupVar" [ "Undefined variable", show (pp x) ]

defLocal :: Name -> Value -> Env -> Env
defLocal x !v env = env { vEnv = Map.insert x v (vEnv env) }

lookupConst :: FName -> Env -> Value
lookupConst f env =
  case Map.lookup f (cEnv env) of
    Just v -> v
    Nothing -> panic "lookupConst" [ "Undefined constant", show (pp f) ]

defConst :: FName -> Value -> Env -> Env
defConst x !v env = env { cEnv = Map.insert x v (cEnv env) }


lookupFun :: FName -> Env -> [Value] -> Value
lookupFun f env =
  case Map.lookup f (fEnv env) of
    Just fv -> fv
    Nothing -> panic "lookupFun"
                  [ "Undefined function", show (pp f), "Known "
                  , show (hsep (punctuate "," (map pp (Map.keys (fEnv env)))))
                  ]

defFun :: FName -> ([Value] -> Value) -> Env -> Env
defFun f v env = env { fEnv = Map.insert f v (fEnv env) }

defFuns :: Map FName ([Value] -> Value) -> Env -> Env
defFuns fs env = env { fEnv = Map.union fs (fEnv env) }

lookupGFun :: FName -> Env -> [Value] -> Parser Value
lookupGFun f env =
  case Map.lookup f (gEnv env) of
    Just fv -> fv
    Nothing -> panic "lookupGFun" [ "Undefined function", show (pp f) ]

defGFuns :: Map FName ([Value] -> Parser Value) -> Env -> Env
defGFuns fs env = env { gEnv = Map.union fs (gEnv env) }

lookupBFun :: FName -> Env -> [Value] -> Word8 -> Bool
lookupBFun f env =
  case Map.lookup f (bEnv env) of
    Just fv -> fv
    Nothing -> panic "lookupBFun" [ "Undefined function", show (pp f) ]

defBFuns :: Map FName ([Value] -> Word8 -> Bool) -> Env -> Env
defBFuns fs env = env { bEnv = Map.union fs (bEnv env) }

lookupType :: TName -> Map TName TDecl -> TDecl
lookupType t env =
  case Map.lookup t env of
    Just d -> d
    Nothing -> panic "lookupType" [ "Undefined type", show (pp t) ]

defTypes :: Map TName TDecl -> Env -> Env
defTypes tys env = env { tEnv = tys `Map.union` tEnv env }
