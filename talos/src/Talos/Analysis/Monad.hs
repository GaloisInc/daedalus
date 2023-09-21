{-# LANGUAGE GADTs, DataKinds, RankNTypes, PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}

{-# LANGUAGE TupleSections #-}
module Talos.Analysis.Monad (getDeclInv, requestSummary, makeDeclInvs, currentDeclName
                            , Summary (..), SummaryClass', Summaries, IterM, AnalysisState(..)
                            , calcFixpoint
                            , module Export) where

import           Control.Monad.Trans     (lift)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromJust, mapMaybe)
import qualified Data.Text               as Text

import           Daedalus.Core
import           Daedalus.GUID           (HasGUID (..))
import           Daedalus.PP             (PP (pp), showPP)
import           Daedalus.Panic          (panic)

import           Talos.Analysis.AbsEnv   (AbsEnv (AbsPred))
import           Talos.Analysis.Domain   (Domain, domainEqv, emptyDomain)
import           Talos.Analysis.Fixpoint as Export (addSummary,
                                                    currentSummaryClass,
                                                    lookupSummary)
import qualified Talos.Analysis.Fixpoint as F
import           Talos.Analysis.Slice    (FInstId (..),
                                          SummaryClass (Assertions),
                                          assertionsFID)
import           Talos.Monad             (TalosM, getModule, LiftTalosM, liftTalosM)

-- import Debug.Trace (trace, traceM)


-- This is the current map from variables to path sets that begin at
-- that variable.  We assume that variables are (globally) unique.
-- type PathRootMap =  Map Name [(FieldSet, Slice)]

type SummaryClass' ae = SummaryClass (AbsPred ae)

-- This is the summarisation for a given class for a given function
data Summary ae =
  Summary { domain      :: Domain ae
          -- Essentially a copy of the params in the decl.
          , params      :: [Name]
          , fInstId     :: FInstId
          }

emptySummary :: FInstId -> Summary ae
emptySummary = Summary emptyDomain []

type Summaries ae = Map FName (Map FInstId (Summary ae))

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

data AnalysisState p = AnalysisState
  { declInvs  :: Map FName (Name -> [Expr] -> (Expr, Expr))
  , summaryToInst :: Map (SummaryClass p) FInstId
  , instToSummary :: Map FInstId (SummaryClass p)
  , nextFInstId   :: Int
  }
  
newtype IterM ae a = IterM
  { getIterM :: F.FixpointT FName FInstId (Summary ae) (AnalysisState (AbsPred ae)) TalosM a
  }
  deriving (Functor, Applicative, Monad)

instance LiftTalosM (IterM ae) where
  liftTalosM = IterM . lift . liftTalosM 
  
instance HasGUID (IterM ae) where
  guidState f = IterM $ lift (guidState f)
  
--------------------------------------------------------------------------------
-- Inverses

-- For now, if we have a function invert_Foo then we call that an inverse
-- for Foo, and similarly for pred_Foo.  This ignores modules.
-- XXX: The mappings here are quite iffy as they are done by name (text)
-- and those are not unique (e.g., specializations and matching functions
-- all have the same text name (but different guids).

makeDeclInvs :: [Fun Grammar] -> [Fun Expr] -> Map FName (Name -> [Expr] -> (Expr, Expr))
makeDeclInvs decls funs = Map.fromList fnsWithInvs
  where
    fnsWithInvs = [ (fName fn, \resN args -> (mkCall fn ifn resN args, mkPred fn t resN args))
                  | fn <- decls
                  , let t = fnameText (fName fn)
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

    isPfx pfx fn = Text.stripPrefix pfx (fnameText fn)

--------------------------------------------------------------------------------
-- low-level IterM primitives

getDeclInv :: FName -> IterM ae (Maybe (Name -> [Expr] -> (Expr, Expr)))
getDeclInv n = IterM $ F.fixpointState (\s -> (Map.lookup n (declInvs s) , s))

getAllocInstId :: Ord (AbsPred ae) => SummaryClass' ae -> IterM ae FInstId
getAllocInstId cl = IterM $ F.fixpointState go
  where
    go st
      | Just fid <- Map.lookup cl (summaryToInst st) = (fid, st)
      | otherwise = mk st

    mk st =
      let fid = FInstId (nextFInstId st)
          st' =  st { nextFInstId = nextFInstId st + 1
                    , summaryToInst = Map.insert cl fid (summaryToInst st)
                    , instToSummary = Map.insert fid cl (instToSummary st)
                    }
      in (fid, st')

getSummaryClass :: FInstId -> IterM ae (SummaryClass' ae)
getSummaryClass fid = IterM $ F.fixpointState go
  where
    go st
      | Just cl <- Map.lookup fid (instToSummary st) = (cl, st)
      | otherwise = panic "Missing inst id." []

-- Gets the precondition for a given decl.  This may update the worklist and revdeps
requestSummary :: AbsEnv ae => FName -> SummaryClass' ae -> IterM ae (Summary ae)
requestSummary nm cl = do
  fid <- getAllocInstId cl
  m_summary <- IterM $ F.requestSummary nm fid
  case m_summary of
    Nothing -> pure $ emptySummary fid
    Just s  -> pure s

calcFixpoint :: (AbsEnv ae, Ord (AbsPred ae)) =>
                (FName -> SummaryClass' ae -> FInstId -> IterM ae (Summary ae)) ->
                F.Worklist FName FInstId ->
                TalosM (AnalysisState (AbsPred ae), Summaries ae)
calcFixpoint m wl = do
  md <- getModule
  let st0 = AnalysisState
            { declInvs      = makeDeclInvs (mGFuns md) (mFFuns md)
            , summaryToInst = Map.singleton Assertions assertionsFID
            , instToSummary = Map.singleton assertionsFID Assertions
            , nextFInstId   = assnId + 1
            }
  F.calcFixpoint seqv go wl st0
  where
    -- Hack
    FInstId assnId = assertionsFID
    
    go n fid = getIterM $ do
      cl <- getSummaryClass fid
      m n cl fid
    seqv oldS newS = domainEqv (domain oldS) (domain newS)

currentDeclName :: IterM ae FName
currentDeclName = IterM F.currentDeclName

-- logMessage :: Int -> String -> IterM ae ()
-- logMessage priority msg = IterM $ F.fixpointState go
--   where
--     go st =
--       ((), st { loggedMessages = (priority, msg) : loggedMessages st })

instance AbsEnv ae => PP (Summary ae) where
  pp s = pp (domain s)



