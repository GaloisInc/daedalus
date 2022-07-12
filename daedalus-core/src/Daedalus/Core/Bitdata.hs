{-# Language ImplicitParams, ConstraintKinds, BlockArguments #-}
-- | Common functionality for compiling bitdata
module Daedalus.Core.Bitdata where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bits(shiftL, (.|.))

import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import qualified Daedalus.BDD as BDD

import Daedalus.Core.Basics
import Daedalus.Core.Type(bdUniverse)
import Daedalus.Core.Decl

-- operations:
-- constructors
-- case/patterns
-- selectors
-- comparisons

-- | Maps type name to (universe, definition)
type HasTDecls = (?tdecls :: Map TName TDecl)

getBD :: HasTDecls => TName -> TDecl
getBD t = case Map.lookup t ?tdecls of
            Just r -> r
            Nothing -> panic "getBD" ["Missing bitdata declaration", show (pp t)]


-- | Expression for an n-bit number representing a bitdata struct.
bdStructConBase :: [BDField] -> Integer
bdStructConBase = foldl setBits 0
  where
  setBits base f =
    case bdFieldType f of
      BDTag x -> base .|. (x `shiftL` bdOffset f)
      _       -> base


bdCase ::
  HasTDecls => TName -> [(Label,a)] -> Maybe a -> [(Integer, [(Integer,a)])]
bdCase t as dflt =
  BDD.groupTestsByMask' (BDD.patTestsAssumingInOrder univ rhs)
  where
  rhs = case dflt of
          Nothing -> map doAlt as
          Just a  -> map doAlt as ++ [ (univ, a) ]


  (univ,cons) = case tDef (getBD t) of
                  TBitdata u (BDUnion fs) -> (u,fs)
                  _ -> panic "bdCase" ["Not a bitdata union"]
  conMap = Map.fromList [ (l, bdUniverse ?tdecls ft) | (l,ft) <- cons ]
  doAlt (l,a) =
    case Map.lookup l conMap of
      Just p  -> (p,a)
      Nothing -> panic "bdCase" ["Missing bitdata constructor: " ++ show (pp l)]


