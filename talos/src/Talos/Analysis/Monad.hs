{-# LANGUAGE GADTs, DataKinds, RankNTypes, KindSignatures, PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Talos.Analysis.Monad where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State

import Daedalus.Core
import Daedalus.PP
import Daedalus.GUID

import Talos.Analysis.Slice
import Talos.Analysis.Domain
import Talos.Analysis.EntangledVars

-- This is the current map from variables to path sets that begin at
-- that variable.  We assume that variables are (globally) unique.
type PathRootMap =  Map Name [(FieldSet, Slice)]

-- This is the summarisation for a given class for a given function
data Summary =
  Summary { exportedDomain   :: Domain
          -- ^ If this is a function result summary, this will contain
          -- an entry for ResultVar, including any params entangled
          -- with the result.
          , pathRootMap :: PathRootMap
          -- Essentially a copy of the params in the decl.
          , params      :: [Name]
          , summaryClass :: SummaryClass 
          }

type Summaries    = Map FName (Map SummaryClass Summary)
-- type Summaries    = Map Name Summary

--------------------------------------------------------------------------------
-- Monad and state

-- This contains the reverse dependencies for each decl (computed on
-- the fly).  It is used to recompute summaries when dependencies
-- change, so the type of summary requested is included.  Note that a
-- name may appear multiole times if multiple (different) summaries
-- are requested.
type RevDeps = Map (FName, SummaryClass) (Set (FName, SummaryClass))
             
type Worklist = Set (FName, SummaryClass)

data IterState =  IterState
  { worklist  :: Worklist
  , revDeps   :: RevDeps
  , summaries :: Summaries
  , allDecls  :: Map FName (Fun Grammar) -- read-only
  , currentDecl  :: FName
  , currentClass :: SummaryClass
  , nextGUID    :: GUID
  }

-- We want assertions for all decls as the base case for calls is to
-- use 'Assertion although we may not always require them (if every
-- call to a function uses the result in a relevant way)
initState :: [Fun Grammar] -> GUID -> IterState
initState decls nguid = IterState
  { worklist    = Set.fromList grammarDecls
  , revDeps     = Map.empty
  , summaries   = Map.empty
  , allDecls    = Map.fromList (map (\tc -> (fName tc, tc)) decls)
  , currentDecl  = error "No current decl"
  , currentClass = error "No current class"
  , nextGUID    = nguid
  }
  where
    grammarDecls =
      [ (name, Assertions)
      | Fun { fName = name, fDef = Def _ } <- decls
      ]

-- We pretend this is a DDL pass to get e.g. fresh name support  
newtype IterM a = IterM { getIterM :: State IterState a }
  deriving (Applicative, Functor, Monad)

runIterM :: IterM a -> IterState -> IterState
runIterM m s0 =  execState (getIterM m) s0

--------------------------------------------------------------------------------
-- low-level IterM primitives

instance HasGUID IterM where
  guidState f = IterM $ state (mkGUIDState' nextGUID (\v s -> s { nextGUID = v }) f)

currentDeclName :: IterM FName
currentDeclName = IterM $ gets currentDecl

currentSummaryClass :: IterM SummaryClass
currentSummaryClass = IterM $ gets currentClass

addRevDep :: FName -> SummaryClass -> IterM ()
addRevDep nm cl = do
  here    <- currentDeclName
  here_cl <- currentSummaryClass
  IterM $ modify (\s -> s { revDeps = Map.insertWith Set.union (nm, cl) (Set.singleton (here, here_cl)) (revDeps s) })

getRevDeps :: FName -> SummaryClass -> IterM (Set (FName, SummaryClass))
getRevDeps nm cl = do
  IterM $ Map.findWithDefault Set.empty (nm, cl) <$> gets revDeps

getDecl :: FName -> IterM (Fun Grammar)
getDecl n = IterM $ gets (flip (Map.!) n . allDecls)

lookupSummary :: FName -> SummaryClass -> IterM (Maybe Summary)
lookupSummary nm cls = do
  m_summary <- IterM $ gets (Map.lookup nm . summaries)
  pure $ Map.lookup cls =<< m_summary 
    
requestSummarisation :: FName -> SummaryClass -> IterM ()
requestSummarisation nm p =
  IterM $ modify (\s -> s { worklist = Set.insert (nm, p) (worklist s) })

-- IterM interface

-- Gets the precondition for a given decl.  This may update the worklist and revdeps
requestSummary :: FName -> SummaryClass -> IterM Summary
requestSummary nm cl = do
  addRevDep nm cl
  m_summary <- lookupSummary nm cl
  case m_summary of
    Nothing -> do requestSummarisation nm cl
                  -- It is OK to return Nothing for the result var
                  pure $ Summary mempty mempty mempty cl
    Just s  -> pure s

-- FIXME: Could make propagateAndGetNext to avoid an insertion

-- propagate notifies callers of a function that its precond has
-- changed, so they need to be resummarised.

-- Note: Everything in revdeps will have a summary
propagate :: FName -> SummaryClass -> IterM ()
propagate nm cl = do
  rdeps  <- getRevDeps nm cl
  IterM $ modify (\s -> s { worklist = Set.union rdeps (worklist s) })

--------------------------------------------------------------------------------
-- Instances

explodePathRootMap :: PathRootMap -> [ (Name, FieldSet, Slice) ]
explodePathRootMap m =
  [ (n, fs, sl) | (n, m') <- Map.toList m, (fs, sl) <- m' ]

instance PP Summary where
  pp s = bullets [ "exported" <+> pp (exportedDomain s)
                 , "internal" <+> vcat (map pp_el (explodePathRootMap (pathRootMap s)))
                 ]
    where
      pp_el (n, fs, sl)
        | fs == emptyFieldSet = pp n <> " => " <> pp sl
        | otherwise           = pp n <> "." <> pp fs <> " => " <> pp sl
