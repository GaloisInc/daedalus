{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-} -- for dealing with TCDecl and existential k

-- We walk through the each decl figuring out if it has things we
-- can't easily handle and thus need to send to the solver.  At the
-- moment this is any Guard.

module Talos.Analysis ( needsSolver
                      --
                      , summarise
                      , Summary
                      -- , Summary(..)
                      -- , Predicate(..)
                      ) where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Functor.Const
import Data.Monoid

import Daedalus.GUID
import Daedalus.Type.AST
import Daedalus.Type.Free
import Daedalus.Type.Traverse

import Talos.Analysis.Monad
import Talos.Analysis.Domain (TCSynthAnnot)
import Talos.Analysis.PathSet

-- import Debug.Trace


-- -----------------------------------------------------------------------------
-- Top level driver

needsSolver :: [TCDecl a] -> Set Name
needsSolver = foldr go Set.empty -- start from top-level functions work to leaves
  where
    go TCDecl {..} s
      | tcDeclName `Set.member` s = addCalls s tcDeclDef
      | isComplex tcDeclDef       = addCalls (Set.insert tcDeclName s) tcDeclDef
      | otherwise                 = s

    addCalls s d = s `Set.union` tcCalls d

-- This function traverses a term and replaces all problematic
-- function calls by speciialised versions
isComplex :: TCDeclDef a k -> Bool
isComplex def =
  case def of
    ExternDecl _ -> error "ExternalDecl"
    Defined d    -> getAny (getConst (go d))
  where
    go :: forall a k'. TC a k' -> Const Any (TC a k')
    go (TC v) = TC <$> traverse go' v

    go' :: forall a k'. TCF a k' -> Const Any (TCF a k')
    go' texpr =
      case texpr of
        TCGuard _b -> Const (Any True)
        -- FIXME: too forceful
        x -> traverseTCF go x


--------------------------------------------------------------------------------
-- Dep analysis

-- Top level function
summarise :: Map TCTyName TCTyDecl ->
             [Name] -> [TCDecl TCSynthAnnot] -> GUID -> (Summaries, GUID)
summarise declTys _roots decls nguid  = (summaries s', nextGUID s')

  where
    s' = calcFixpoint s0 
    s0 = initState declTys decls nguid 
