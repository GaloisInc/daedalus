{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}

{-# LANGUAGE TupleSections #-}
module Talos.Analysis.Monad (getDeclInv, requestSummary, initState, makeDeclInvs
                            , Summary (..), SummaryClass', Summaries, IterM, AnalysisState(..)
                            , calcFixpoint
                            , module Export) where

import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust, mapMaybe)
import qualified Data.Text                    as Text

import           Daedalus.Core
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic

import           Talos.Analysis.Domain
import           Talos.Analysis.Fixpoint      as Export (addSummary,
                                                         currentDeclName,
                                                         currentSummaryClass,
                                                         lookupSummary)
import qualified Talos.Analysis.Fixpoint      as F
import           Talos.Analysis.Slice

-- This is the current map from variables to path sets that begin at
-- that variable.  We assume that variables are (globally) unique.
-- type PathRootMap =  Map Name [(FieldSet, Slice)]

type SummaryClass' ae = SummaryClass (AbsPred ae)

-- This is the summarisation for a given class for a given function
data Summary ae =
  Summary { domain   :: Domain ae
          -- Essentially a copy of the params in the decl.
          , params      :: [Name]
          , summaryClass :: SummaryClass' ae
          }


emptySummary :: SummaryClass' ae -> Summary ae
emptySummary = Summary emptyDomain []

type Summaries ae = Map FName (Map (SummaryClass' ae) (Summary ae))

--------------------------------------------------------------------------------
-- Instances

-- explodePathRootMap :: PathRootMap -> [ (Name, FieldSet, Slice) ]
-- explodePathRootMap m =
--   [ (n, fs, sl) | (n, m') <- Map.toList m, (fs, sl) <- m' ]

-- instance PP Summary where
--   pp s = bullets [ "exported" <+> pp (exportedDomain s)
--                  , "internal" <+> vcat (map pp_el (explodePathRootMap (pathRootMap s)))
--                  ]
--     where
--       pp_el (n, fs, sl)
--         | fs == emptyFieldSet = pp n <> " => " <> pp sl
--         | otherwise           = pp n <> "." <> pp fs <> " => " <> pp sl

--------------------------------------------------------------------------------
-- Monad and state

-- This contains the reverse dependencies for each decl (computed on
-- the fly).  It is used to recompute summaries when dependencies
-- change, so the type of summary requested is included.  Note that a
-- name may appear multiole times if multiple (different) summaries
-- are requested.

data AnalysisState = AnalysisState
  { declInvs  :: Map FName (Name -> [Expr] -> (Expr, Expr))
  , nextGUID  :: GUID
  }

-- We want assertions for all decls as the base case for calls is to
-- use 'Assertion although we may not always require them (if every
-- call to a function uses the result in a relevant way)
initState :: [Fun Grammar] -> [Fun Expr] -> GUID -> AnalysisState
initState decls funs nguid = AnalysisState
  { declInvs    = makeDeclInvs decls funs
  , nextGUID    = nguid
  }

newtype IterM ae a = IterM { getIterM :: F.FixpointM FName (SummaryClass' ae) (Summary ae) AnalysisState a}
  deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------
-- Inverses

-- For now, if we have a function invert_Foo then we call that an inverse
-- for Foo, and similarly for pred_Foo.  This ignores modules.
makeDeclInvs :: [Fun Grammar] -> [Fun Expr] -> Map FName (Name -> [Expr] -> (Expr, Expr))
makeDeclInvs decls funs = Map.fromList fnsWithInvs
  where
    fnsWithInvs = [ (fName fn, \resN args -> (mkCall fn ifn resN args, mkPred fn t resN args))
                  | fn <- decls
                  , Just t <- [fnameText (fName fn)]
                  , Just ifn <- [Map.lookup t inverses]
                  ]

    inverses = mapByPfx "inverse_"
    preds    = mapByPfx "pred_"

    mkPred fn fnText
      | Just pfn <- Map.lookup fnText preds = mkCall fn pfn
      | otherwise = \_ _ -> boolL True

    -- FIXME: inefficient, move the lambda somehow?
    mkCall fn pfn = \resN args ->
      -- We line up arguments by source name
      -- FIXME: type check this!
      let identMap = Map.fromList [ (n, e)
                                  | (pn, e) <- zip (fParams fn) args
                                  , Just n <- [nameText pn]
                                  ]

          -- Should only be the final argument which doesn't line up
          (_resA, ps') = case reverse (fParams pfn) of
            [] -> panic "Need more arguments" [showPP (fName pfn)]
            resP : psR -> (resP, reverse psR)

          args' = map ((Map.!) identMap . fromJust . nameText) ps'
      in callF (fName pfn) (args' ++ [ Var resN ])

    mapByPfx pfx = Map.fromList (mapMaybe (\f -> (, f) <$> isPfx pfx (fName f)) funs)

    isPfx pfx fn = Text.stripPrefix pfx =<< fnameText fn

--------------------------------------------------------------------------------
-- low-level IterM primitives

instance HasGUID (IterM ae) where
  guidState f = IterM $ F.fixpointState (mkGUIDState' nextGUID (\v s -> s { nextGUID = v }) f)

getDeclInv :: FName -> IterM ae (Maybe (Name -> [Expr] -> (Expr, Expr)))
getDeclInv n = IterM $ F.fixpointState (\s -> (Map.lookup n (declInvs s) , s))

-- Gets the precondition for a given decl.  This may update the worklist and revdeps
requestSummary :: Ord (AbsPred ae) => FName -> SummaryClass (AbsPred ae) -> IterM ae (Summary ae)
requestSummary nm cl = IterM $ do
  m_summary <- F.requestSummary nm cl
  case m_summary of
    Nothing -> -- It is OK to return Nothing for the result var
      pure $ emptySummary cl
    Just s  -> pure s

calcFixpoint :: (AbsEnv ae, Ord (AbsPred ae)) =>
                (FName -> SummaryClass' ae -> IterM ae (Summary ae)) ->
                F.Worklist FName (SummaryClass' ae) ->
                AnalysisState -> (AnalysisState, Summaries ae)
calcFixpoint m wl = F.calcFixpoint seqv (\n cl -> getIterM (m n cl)) wl
  where
    seqv oldS newS = domainEqv (domain oldS) (domain newS)
