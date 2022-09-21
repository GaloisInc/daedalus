{-# Language TemplateHaskell #-}
{-# Language ImplicitParams #-}
{-# Language ConstraintKinds #-}
module Daedalus.Core.TH.Type (compileMonoType, compileType, HasTypeParams) where

import Data.Map(Map)
import qualified Data.Map as Map

import qualified Daedalus.RTS as RTS
import qualified Daedalus.RTS.Vector as RTS
import qualified Daedalus.RTS.Map as RTS

import Daedalus.TH(Q)
import qualified Daedalus.TH as TH
import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import Daedalus.Core.Basics

import Daedalus.Core.TH.Names


compileMonoType :: Type -> Q TH.Type
compileMonoType =
  let ?typeParams = mempty
  in compileType

type HasTypeParams = (?typeParams :: Map TParam (Q TH.Type))

compileType :: HasTypeParams => Type -> Q TH.Type
compileType ty =
  case ty of
    TStream     -> [t| RTS.Input |]
    TUInt sz    -> [t| RTS.UInt $(compileSizeType sz) |]
    TSInt sz    -> [t| RTS.SInt $(compileSizeType sz) |]
    TInteger    -> [t| Integer |]
    TBool       -> [t| Bool |]
    TFloat      -> [t| Float |]
    TDouble     -> [t| Double |]
    TUnit       -> [t| () |]
    TArray t    -> [t| RTS.Vector $(compileType t) |]
    TMaybe t    -> [t| Maybe $(compileType t) |]
    TMap k v    -> [t| RTS.Map $(compileType k) $(compileType v) |]
    TBuilder t  -> [t| RTS.Builder $(compileType t) |]
    TUser ut    -> foldl TH.appT (TH.conT (dataName (utName ut)))
                     $ [ compileSizeType t | t <- utNumArgs ut ]
                    ++ [ compileType     t | t <- utTyArgs ut ]
    TIterator t -> [t| RTS.Iterator $(compileType t) |]
    TParam p    -> compileTypeParam p

compileSizeType ::
  (?typeParams :: Map TParam (Q TH.Type)) => SizeType -> Q TH.Type
compileSizeType sz =
  case sz of
    TSize n      -> TH.litT (TH.numTyLit n)
    TSizeParam p -> compileTypeParam p

compileTypeParam ::
  (?typeParams :: Map TParam (Q TH.Type)) => TParam -> Q TH.Type
compileTypeParam p =
  case Map.lookup p ?typeParams of
    Just t  -> t
    Nothing -> panic "compileSizeType" [ "Missing type parameter", show (pp p) ]


