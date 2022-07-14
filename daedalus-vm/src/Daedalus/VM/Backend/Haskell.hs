{-# Language TemplateHaskell, ConstraintKinds, ImplicitParams #-}
module Daedalus.VM.Backend.Haskell where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import qualified Data.Text as Text
import qualified Language.Haskell.TH.Lib as TH
import GHC.Records(getField)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import qualified RTS          as RTS
import qualified RTS.Input    as RTS
import qualified RTS.Numeric  as RTS
import qualified RTS.Vector   as RTS
import qualified RTS.Iterator as RTS
import qualified RTS.ParserVM as RTS

import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import Daedalus.Core(Type(..),tByteArray,tWord)
import Daedalus.Core.Type(typeOf)
import Daedalus.Core.TH.Type
import Daedalus.Core.TH.Ops
import Daedalus.VM

data Config = Config
  { userMonad      :: TH.TypeQ
  , userPrimitives :: Map FName ([TH.ExpQ] -> TH.ExpQ)
  }

-- | Make a function with the given list of arguments and result.
funT :: [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
funT args res = foldr addArg res args
  where
  addArg x y = [t| $x -> $y |]

type HasConfig = (?config :: Config)

userMonadT :: HasConfig => TH.TypeQ
userMonadT = userMonad ?config

codeT :: HasConfig => TH.TypeQ -> TH.TypeQ
codeT r = [t| RTS.ThreadState $r $userMonadT |]

-- | The type of a capturing parser
cParserT :: HasConfig => [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ -> TH.TypeQ
cParserT as b r = funT as [t| RTS.CParser $r $userMonadT $b |]

-- | The type of a block in a capturing parser
cParserBlockT :: HasConfig => [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
cParserBlockT as r = funT as (codeT r)

-- | The type of a non-capturing parser.
dParserT :: HasConfig => [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
dParserT as b = funT as [t| RTS.DParser $userMonadT $b |]


--------------------------------------------------------------------------------

type BlockEnv =
  ( ?blockArgs :: Map BA TH.ExpQ
  , ?localVars :: Map BV TH.ExpQ
  )

compileE :: BlockEnv => E -> TH.ExpQ
compileE expr =
  case expr of
    EUnit         -> [| () |]
    ENum i t      -> [| i :: $(compileMonoType t) |]
    EBool b       -> [| b |]
    EFloat d t    -> [| d :: $(compileMonoType t) |]
    EMapEmpty k v -> [| mempty  :: $(compileMonoType (TMap k v)) |]
    ENothing t    -> [| Nothing :: Maybe $(compileMonoType t) |]
    EBlockArg ba  ->
      case Map.lookup ba ?blockArgs of
        Just e -> e
        Nothing -> panic "compileE" ["Missing argument", show (pp ba)]
    EVar bv ->
      case Map.lookup bv ?localVars of
        Just e -> e
        Nothing -> panic "compileE" ["Missing local variable", show (pp bv)]

compilePrim :: BlockEnv => PrimName -> [E] -> TH.ExpQ
compilePrim prim es =
  let args = map compileE es
  in
  case prim of

    StructCon ut   -> undefined

    NewBuilder t   -> [| RTS.emptyBuilder :: $(compileMonoType (TBuilder t)) |]

    Integer i      -> [| i :: Integer |]

    ByteArray bs   -> [| RTS.vecFromRep $lit :: $(compileMonoType tByteArray) |]
      where lit = [| $(TH.litE (TH.stringL (BS8.unpack bs))) :: BS.ByteString |]

    Op1 op1 ->
      case args of
        [e] -> compileOp1 op1 (getSemType (head es)) e
        _   -> panic "compilePrim" ["Op1 arity mismatch"]


    Op2 op2 ->
      case args of
        [e1,e2] -> compileOp2 op2 e1 e2
        _       -> panic "compilePrim" ["Op2 arity mismatch"]

    Op3 op3 ->
      case args of
        [e1,e2,e3] -> compileOp3 op3 e1 e2 e3
        _          -> panic "compilePrim" ["Op3 arity mismatch"]

    OpN opN -> compileOpN doFun opN args
      where doFun = panic "compileOpN" ["Unexpcetd function call"]



