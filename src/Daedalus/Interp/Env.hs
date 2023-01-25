module Daedalus.Interp.Env where

import Data.Map (Map)
import qualified Data.Map as Map

import RTS.ParseError
import RTS.Parser
import RTS.ParserAPI

import qualified Daedalus.AST as K
import Daedalus.Type.AST(TVar,TCTyName, TCTyDecl,TCName, Name, tcName)
import Daedalus.Value
import Daedalus.Interp.DebugAnnot

type Parser     = ParserG     DebugAnnot
type ParseError = ParseErrorG DebugAnnot
type Result     = ResultG     DebugAnnot

data SomeVal      = VVal Value | VClass ClassVal | VGrm (PParser Value)
data SomeFun      = FVal (Fun Value)
                  | FClass (Fun ClassVal)
                  | FGrm (Fun (Parser Value))
newtype Fun a     = Fun ([TValue] -> [SomeVal] -> a)

instance Show (Fun a) where
  show _ = "FunDecl"

type PParser a  = [SomeVal] -> Parser a



data Env = Env
  { ruleEnv :: Map Name (Fun (Parser Value))
  , funEnv  :: Map Name (Fun Value)
  , clsFun  :: Map Name (Fun ClassVal)

  , valEnv   :: Map Name Value
  , clsEnv   :: Map Name ClassVal
  , gmrEnv   :: Map Name (PParser Value)

  , tyEnv   :: Map TVar TValue
    -- ^ Bindings for polymorphic type argumens
  , tyDecls :: Map TCTyName TCTyDecl
    -- ^ Used for bitdata (for coercion)
  }

type Prims = Map Name SomeFun

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty
               Map.empty Map.empty Map.empty
               Map.empty Map.empty

setVals :: Map Name Value -> Env -> Env
setVals vs env = env { valEnv = vs }

addVal :: TCName K.Value -> Value -> Env -> Env
addVal x v env = env { valEnv = Map.insert (tcName x) v (valEnv env) }

addValMaybe :: Maybe (TCName K.Value) -> Value -> Env -> Env
addValMaybe Nothing  _ e = e
addValMaybe (Just x) v e = addVal x v e

pScope :: Env -> Parser a -> Parser a
pScope env =
  let venv = valEnv env
  in if Map.null venv then id else pEnter (ScopeAnnot venv)


