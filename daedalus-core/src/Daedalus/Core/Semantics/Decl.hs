{-# Language BlockArguments #-}
module Daedalus.Core.Semantics.Decl where

import qualified Data.Map as Map
import Data.Either(partitionEithers)
import Data.List(foldl')
import Data.Word(Word8)

import RTS.Parser(Parser)
import Daedalus.Rec(topoOrder)
import Daedalus.PP(pp)
import Daedalus.Panic(panic)

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.Semantics.Value
import Daedalus.Core.Semantics.Env
import Daedalus.Core.Semantics.Expr
import Daedalus.Core.Semantics.Grammar


evalModule :: Module -> Env -> Env
evalModule m = evalGFuns (mGFuns m) . evalBFuns (mBFuns m) . evalFuns (mFFuns m)

evalFuns :: [Fun Expr] -> Env -> Env
evalFuns fns0 env0 = foldl' evalRec env0 (topoOrder deps fns0)
  where
  deps f = (fName f, freeFVars f)

  evalRec env r =
    case r of
      NonRec fn ->
        case evalFun fn env of
          Left (f,v)  -> defConst f v env
          Right (f,v) -> defFun   f v env

      MutRec fns ->
        let (cs,fs) = partitionEithers [ evalFun f newEnv | f <- fns ]
            newEnv  = defFuns (Map.fromList fs) env
        in case cs of
             [] -> newEnv
             _  -> panic "evalFun" [ "Recursive constants:"
                                   , unwords (map (show . pp . fst) cs)
                                   ]

evalFun :: Fun Expr -> Env -> Either (FName, Value) (FName, [Value] -> Value)
evalFun fn env =
  case fParams fn of
    [] -> Left  (fName fn, val [])
    _  -> Right (fName fn, val)
  where
  val = \vs -> let env1 = foldr (uncurry defLocal) env (zip (fParams fn) vs)
               in case fDef fn of
                    Def e    -> eval e env1
                    External -> lookupFun (fName fn) env vs


evalGFuns :: [Fun Grammar] -> Env -> Env
evalGFuns fns env = newEnv
  where
  newEnv = defGFuns (Map.fromList [ evalGFun f newEnv | f <- fns ]) env

evalGFun :: Fun Grammar -> Env -> (FName, [Value] -> Parser Value)
evalGFun fn env = (fName fn, val)
  where
  val = \vs -> let env1 = foldr (uncurry defLocal) env (zip (fParams fn) vs)
               in case fDef fn of
                    Def e    -> evalG e env1
                    External -> lookupGFun (fName fn) env vs


evalBFun :: Fun ByteSet -> Env -> (FName, [Value] -> Word8 -> Bool)
evalBFun fn env = (fName fn, val)
  where
  val = \vs -> let env1 = foldr (uncurry defLocal) env (zip (fParams fn) vs)
               in case fDef fn of
                    Def e    -> evalByteSet e env1
                    External -> lookupBFun (fName fn) env vs


evalBFuns :: [Fun ByteSet] -> Env -> Env
evalBFuns fns env = newEnv
  where
  newEnv = defBFuns (Map.fromList [ evalBFun f newEnv | f <- fns ]) env
  -- these are not recursive, but in this way we don't need to order them.
