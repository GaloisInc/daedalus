{-# Language BlockArguments #-}
module Reference where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable(toList)

import AST
import Value

type Parser = I -> Res
type Res    = Either Int (NonEmpty (V,I))

jnRes2 :: Res -> Res -> Res
jnRes2 l r =
  case (l,r) of
    (Right xs, Right ys) -> Right (xs <> ys)
    (Left x, Left y)     -> Left (max x y)
    (Left _, Right ys)   -> Right ys
    (Right xs, Left _)   -> Right xs



eval :: E -> Env -> V
eval expr env =
  case expr of
    Var x    -> lookupLocal x env
    Char c   -> VChar c
    Int i    -> VInt i
    App f es -> lookupFun f env [ eval e env | e <- es ]




compile :: P -> GEnv -> Parser
compile expr env =
    case expr of

      Call r es ->
        lookupRule r env [ evalE e env | e <- es ]

      Do x p q ->
        \i -> case compile p env i of
                Left err -> Left err
                Right rs ->
                  foldr1 jnRes2
                      [ compile q (gdef x v env)  j | (v,j) <- toList rs ]

      p :|| q -> \i -> jnRes2 (compile p env i) (compile q env i)

      p :<| q ->
        \i -> case compile p env i of
                Left _   -> compile q env  i
                Right rs -> Right rs

      If e p q ->
        case evalE e env of
          VBool b -> if b then compile p env
                          else compile q env
          _ -> error "Type error: `if` expects a boolean"

      Pure e -> \i -> Right $ (evalE e env,i) :| []

      Fail _ ->  \i -> Left (loc i)

      Peek -> \i -> Right $ (VInput i,i) :| []

      SetInput e ->
        case evalE e env of
          VInput i -> \_ -> Right $ (VUnit,i) :| []
          _ -> error "Type error: expected an input"



compileFun :: Fun -> GEnv -> (F, [V] -> Parser)
compileFun (Fun f xs p) env =
  (f, \vs -> let env1 = foldr (uncurry gdef) env (zip xs vs)
             in compile p env1)

compileFuns :: [Fun] -> GEnv
compileFuns fs = env
  where
  env = GEnv { vEnv = emptyEnv
             , gEnv = Map.fromList [ compileFun f env | f <- fs ]
             }



parse :: ([Fun],P) -> I -> Either Int (NonEmpty (V,I))
parse (fs,p) x = compile p (compileFuns fs) x


--------------------------------------------------------------------------------
data GEnv = GEnv
  { vEnv :: Env
  , gEnv :: Map F ([V] -> Parser)
  }

evalE :: E -> GEnv -> V
evalE e env = eval e (vEnv env)

gdef :: N -> V -> GEnv -> GEnv
gdef x v env = env { vEnv = def x v (vEnv env) }

lookupRule :: F -> GEnv -> [V] -> Parser
lookupRule f env =
  case Map.lookup f (gEnv env) of
    Just r -> r
    Nothing -> error ("Undefined rule: " ++ show f)


data Env = Env
  { envLocals :: Map N V
  , envFuns   :: Map F FunV
  }

emptyEnv :: Env
emptyEnv = Env
  { envLocals = Map.empty
  , envFuns   = Map.fromList primFuns
  }

def :: N -> V -> Env -> Env
def x v env = env { envLocals = Map.insert x v (envLocals env) }

lookupLocal :: N -> Env -> V
lookupLocal x env =
  case Map.lookup x (envLocals env) of
    Just v  -> v
    Nothing -> error $ unlines $ ("Undefined variable `" ++ show x ++ "`")
                               : map show (Map.toList (envLocals env))

lookupFun :: F -> Env -> [V] -> V
lookupFun x env =
  case Map.lookup x (envFuns env) of
    Just v  -> v
    Nothing -> error ("Undefined function `" ++ show x ++ "`")
-------------------------------------------------------------------------------




