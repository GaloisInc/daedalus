{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- Calculate the bits of the format which are not in a slice.

module Talos.Residuals (residuals) where

import           Control.Lens            (at, (&), (.=), (.~), (<>~))
import           Control.Monad.State     (StateT, gets, runStateT)

import           Data.Generics.Labels    ()
import           Data.Proxy              (Proxy (..))
import           Data.String             (fromString)
import           GHC.Generics            (Generic)

import           Control.Monad           (zipWithM)
import           Control.Monad.Reader    (ReaderT, asks, local, runReaderT)
import           Data.Map                (Map)
import qualified Data.Map                as Map

import           Daedalus.Core
import           Daedalus.GUID           (invalidGUID)
import           Daedalus.Panic          (panic)
import           Daedalus.PP             (showPP)

import           Talos.Analysis          (summarise)
import           Talos.Analysis.Domain   (Slice, callClass, closedElements,
                                          elements, gsSlice)
import           Talos.Analysis.FLAbsEnv (FLAbsEnv)
import           Talos.Analysis.Merge    (merge)
import           Talos.Analysis.Monad    (Summaries, domain)
import           Talos.Analysis.Slice    (FInstId, Slice' (..), assertionsFID,
                                          sloopClassBody)
import           Talos.Monad             (TalosM, getGFun)

data ResidualState = ResidualState
   { worklist :: Map (FName, FInstId) FName
   , seen     :: Map (FName, FInstId) FName
   
   -- Read only
   , nonResidual :: Grammar -- ^ The replacement for (non-composite) grammars that are in slices
   , summaries :: Summaries FLAbsEnv
   } deriving (Generic)

data ResidualEnv = ResidualEnv {
  -- Copied from synthesis
  pathSetRoots :: Map Name [Slice]
  , currentClass :: FInstId
  , currentFName :: FName
  }

type ResidualM a = ReaderT ResidualEnv (StateT ResidualState TalosM) a

residuals :: FName -> TalosM [Fun Grammar]
residuals entry = do
  ss <- summarise (Proxy @FLAbsEnv)

  nonResidualFn <- freshFName placeHolderName
  let st0 = ResidualState { worklist = Map.singleton (entry, assertionsFID) entry
                          , seen = Map.empty
                          , nonResidual = Call nonResidualFn []
                          , summaries = ss
                          }
  go [] st0
  where
    go acc st
      | Just (((fn, fid), fn'), rest) <- Map.minViewWithKey (worklist st) = do
          (r, st') <- runStateT (runReaderT (residualsF fn fid fn') env0) (st & #worklist .~ rest) 
          go (r : acc) st'
          
      | otherwise = pure acc

    env0 = error "Empty env" -- A bit of a hack, means the types are a bit cleaner

    placeHolderName = FName { fnameId = invalidGUID
                            , fnameText = "XXX"
                            , fnamePublic = False
                            , fnameType = TUnit
                            , fnameMod  = fnameMod entry
                            }
    
residualsF :: FName -> FInstId -> FName -> ResidualM (Fun Grammar)
residualsF fn fid newfn = do
  gfun <- getGFun fn
  summary <- gets (domain . flip (Map.!) fid . flip (Map.!) fn . summaries)
  let env = ResidualEnv { pathSetRoots = closedElements summary
                        , currentClass = fid
                        , currentFName = fn
                        }
      sl = foldl merge SHole (map gsSlice (elements summary))
      
  case fDef gfun of
    External -> panic "External function" []
    Def g    -> do
      g' <- local (const env) (residualsG sl g)
      pure (gfun { fName = newfn, fDef = Def g' })

internalSlice :: Slice -> Name -> ResidualM Slice
internalSlice sl x = do
  m_sl <- asks (Map.lookup x . pathSetRoots)
  pure $ case m_sl of
    Nothing  -> sl
    Just sls -> foldl merge sl sls

residualsDo :: Slice -> Maybe Name -> Grammar -> Grammar -> ResidualM Grammar
residualsDo sl m_x lhs rhs = do
  sl' <- maybe (pure sl) (internalSlice sl) m_x
  case sl' of
    SHole -> mkdo <$> residualsG SHole lhs <*> residualsG SHole rhs
    SDo _ slhs srhs -> mkdo <$> residualsG slhs lhs <*> residualsG srhs rhs
    _ -> panic "Unexpected slice shape" []
  where
    mkdo | Just x <- m_x = Do x
         | otherwise     = Do_

residualsCall :: FName -> FInstId -> [Expr] -> ResidualM Grammar
residualsCall fn fid args = do
  cs <- gets (Map.lookup (fn, fid) . seen)
  fn' <- case cs of
           Just fn' -> pure fn'
           Nothing  -> do
             let fn0' = fn & #fnameText <>~ fromString ("::" <> showPP fid)
             fn' <- freshFName fn0'
             #seen     . at (fn, fid) .= Just fn'
             #worklist . at (fn, fid) .= Just fn'
             pure fn'
  pure (Call fn' args)

-- This will produce things like (Do n placeholder placeholder)
-- etc. which we may want to detect and replace with a single placeholder.
residualsG :: Slice -> Grammar -> ResidualM Grammar
residualsG sl g =
  case (sl, g) of
    (_, Pure {}) -> pure g -- We don't touch Pure
    (SMatch bs, Match {}) -> placeholder
    (_, Do x lhs rhs) -> residualsDo sl (Just x) lhs rhs
    (_, Do_ lhs rhs)  -> residualsDo sl Nothing lhs rhs
    (_, Let n e rhs)  -> residualsG sl (Do n (Pure e) rhs) -- Hack, follows synthesis

    (SChoice sls, Choice biased gs) -> do
      Choice biased <$> zipWithM residualsG sls gs
    (SHole, Choice biased gs) -> do
      Choice biased <$> mapM (residualsG SHole) gs

    -- This overapproximates the body of the call as not every call
    -- site will use every exported slice (I think?).  We could also
    -- pass in callSlices, which is empty for the Hole case.
    (SCall cn, Call fn args) -> residualsCall fn (callClass cn) args
    (SHole, Call fn args)    -> residualsCall fn assertionsFID args

    (_, Annot a g') -> Annot a <$> residualsG sl g'

    (SCase _total (Case _ spats), GCase (Case n pats)) -> do
      let go (p1, sl') (p2, g')
            | p1 /= p2 = panic "Unexpected pattern mismatch" []
            | otherwise = (,) p2 <$> residualsG sl' g'
      GCase . Case n <$> zipWithM go spats pats
    (SHole, GCase cs) -> GCase <$> traverse (residualsG SHole) cs
    (SHole, Loop lc) ->
      Loop <$> traverse (residualsG SHole) lc

    (SLoop slc, Loop lc) ->
      Loop <$> traverse (residualsG (sloopClassBody slc)) lc

    (SInverse {}, _) -> panic "Unverses are not supported" []
    (SHole, _) -> pure g -- Atomic statements
    _ -> panic "Unexpected slice/grammar combo" []
    
  where
    placeholder = gets nonResidual
    
