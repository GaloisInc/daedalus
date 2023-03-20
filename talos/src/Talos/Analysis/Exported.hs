{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- During analysis it is handy to have call nodes track sets of
-- slices, but afterwards we need to have call sites target a single
-- (merged) slice.  When we are exporting domains (forgetting abstract
-- envs, for example) we simplify calls to target a single slice by
-- merging as appropriate.
module Talos.Analysis.Exported
  ( exportSummaries, ExpSlice, SliceId
  , ExpSummaries(..), ExpSummary, ExpCallNode(..)
  , sliceToCallees
  ) where

import           Control.Lens                    (at, (.=))
import           Control.Lens.Lens               ((<<+=))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Generics.Product           (field)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes)
import           Data.Monoid                     (Any (..))
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           GHC.Generics                    (Generic)

import           Daedalus.Core                   (Case (Case), Expr, FName,
                                                  LoopClass' (..),
                                                  LoopMorphism' (..), Name,
                                                  TDecl, TName, loopClassBody,
                                                  nameId, LoopCollection'(..))
import           Daedalus.Core.Basics            (freshName)
import           Daedalus.Core.Expr              (Expr (Var))
import           Daedalus.Core.Free
import           Daedalus.Core.Subst             (Subst, substitute)
import           Daedalus.Core.TraverseUserTypes
import           Daedalus.GUID                   (GUID, HasGUID, guidState,
                                                  mkGUIDState')
import           Daedalus.PP
import           Daedalus.Panic                  (panic)
import           Daedalus.Rec                    (Rec (..), topoOrder)

import           Talos.Analysis.Domain           (CallNode (..), Domain, Slice,
                                                  closedElements, elements,
                                                  gsSlice)
import           Talos.Analysis.Merge            (merge)
import           Talos.Analysis.Monad            (Summaries, Summary (..))
import           Talos.Analysis.SLExpr           (slExprToExpr')
import           Talos.Analysis.Slice            (FInstId, Slice' (..))

--------------------------------------------------------------------------------
-- Types

data ExpCallNode = ExpCallNode
  { ecnName     :: FName
  , ecnIdx      :: FInstId
  -- ^ ecnName and ecnIdx are used to get the root slices.
  , ecnSliceId  :: SliceId
  , ecnParamMap :: Map Name Name
  }

type ExpSlice = Slice' ExpCallNode Expr

type ExpSummary = Map Name [SliceId]

data ExpSummaries = ExpSummaries
  { esRootSlices     :: Map FName (Map FInstId ExpSummary)
  , esFunctionSlices :: Map SliceId ExpSlice
  -- These possibly don't belong here (maybe in StrategyM?)
  , esRecs           :: Map SliceId (Set SliceId)
  -- ^ For each recursive slice, this gives the SCC it belongs to.
  , esRecVars        :: Set Name
  -- ^ These variables contain a recursive call.
  , esBackEdges :: Map SliceId (Map SliceId (Set SliceId))
  -- ^ Maps SCC entry point to the set of back edges in that group.
  }

--------------------------------------------------------------------------------
-- Extra meta-data about slices

sliceToCallees :: ExpSlice -> Set SliceId
sliceToCallees = go
  where
    go sl = case sl of
      SHole {}          -> mempty
      SPure {}          -> mempty
      SDo _ l r         -> go l <> go r
      SMatch _m         -> mempty
--      SAssertion _e     -> mempty
      SChoice cs        -> foldMap go cs
      SCall cn          -> Set.singleton (ecnSliceId cn)
      SCase _ c         -> foldMap go c
      SLoop _ lc        -> go (loopClassBody lc)
      SInverse {}       -> mempty -- No grammar calls

makeEsRecs :: Map SliceId ExpSlice ->  Map SliceId (Set SliceId)
makeEsRecs sls =
  Map.fromList [ (e, scc)
               | MutRec es <- recs
               , let scc = Set.fromList (map fst es)
               , (e, _) <- es
               ]
  where
    recs = topoOrder edges (Map.toList sls)
    edges (sid, sl) = (sid, sliceToCallees sl)

makeEsRecVars :: Map SliceId ExpSlice -> Map SliceId (Set SliceId) -> Set Name
makeEsRecVars slm = Map.foldMapWithKey go
  where
    go sid recs | Just sl <- Map.lookup sid slm = sliceToRecVars recs sl
    go _   _ = panic "Missing slice" []

backEdgesForNode :: Map SliceId ExpSlice -> SliceId -> Set SliceId -> Map SliceId (Set SliceId)
backEdgesForNode sls rootId sccs = evalState (go mempty rootId) mempty
  where
    go :: [SliceId] -> SliceId -> State (Set SliceId) (Map SliceId (Set SliceId))
    go ancestors nodeId = do
      seen <- get
      put (Set.insert nodeId seen)
      let node = sls Map.! nodeId
          children = sliceToCallees node `Set.intersection` sccs
          go1 childId
            | childId `elem` ancestors = pure (Map.singleton nodeId (Set.singleton childId))
            | childId `Set.member` seen = pure Map.empty
            | otherwise = go (nodeId : ancestors) childId
      
      Map.unionsWith (<>) <$> mapM go1 (Set.toList children)

makeEsBackEdges :: Map SliceId ExpSlice -> Map SliceId (Set SliceId) -> Map SliceId (Map SliceId (Set SliceId))
makeEsBackEdges sls = Map.mapWithKey (backEdgesForNode sls)

-- | Gives the set of variables which are bound to grammars which may
-- call recursively
sliceToRecVars :: Set SliceId -> ExpSlice -> Set Name
sliceToRecVars recs = snd . go
  where
    go :: ExpSlice -> (Any, Set Name)
    go sl = case sl of
      SHole    -> mempty
      SPure {} -> mempty
      SDo x l r ->
        let (l_rec, ls) = go l
            (r_rec, rs) = go r
        in ( l_rec <> r_rec
           , ls <> rs <> (if getAny l_rec then Set.singleton x else mempty)
           )
           
      SMatch {}  -> mempty
      SChoice cs -> foldMap go cs
      SCall cn -> (Any $ ecnSliceId cn `Set.member` recs, mempty)
      SCase _ cs -> foldMap go cs
      SLoop _ lc  -> go (loopClassBody lc)
      SInverse {} -> mempty
    
--------------------------------------------------------------------------------
-- Monad

-- We need to do the following for each slice:
--  - create a slice for each callsite target
--  - instantiate the holes in slexprs to get regular exprs.
--  - Make all Do bound variables unique.
--
-- It is more or less easy to do this in one step.

newtype SliceId = SliceId { getSliceId :: Int }
  deriving (Eq, Ord, Show)

instance PP SliceId where
  pp = pp . getSliceId

data ExpState ae = ExpState
  { seenNames      :: Set Name -- ^ Globally seen (grammar) names
                                  -- for the renaming part
  , worklist       :: Worklist
  , sliceMap       :: Map (FName, FInstId, Set Int) SliceId
  , slices         :: Map SliceId ExpSlice
  , nextGUID       :: GUID
  , nextSliceId    :: Int
  -- Read only
  , summaries      :: Summaries ae
  , tyEnv          :: Map TName TDecl
  } deriving Generic

-- This is the rename map for making bound variables unique across slices.
type ExpEnv = Map Name Name

newtype ExpM ae a = ExpM { getExpM :: ReaderT ExpEnv (State (ExpState ae)) a }
  deriving (Functor, Applicative, Monad, MonadReader ExpEnv, MonadState (ExpState ae))

instance HasGUID (ExpM ae) where
  guidState f = state (mkGUIDState' nextGUID (\v s -> s { nextGUID = v }) f)

runExpM :: ExpM ae a -> ExpState ae -> (a, ExpState ae)
runExpM m = runState (runReaderT (getExpM m) Map.empty)

getSummary :: FName -> FInstId -> ExpM ae (Summary ae)
getSummary fn fid = do
  summs <- gets summaries
  let summ = do
        insts <- Map.lookup fn summs
        Map.lookup fid insts
  case summ of
    Just s -> pure s
    Nothing -> panic "Missing summary" []

-- Will add to the worklist if we don't know anything about the slice.
getAllocSliceId :: (FName, FInstId, Set Int) -> ExpM ae SliceId
getAllocSliceId wle = do
  nsl <- gets (SliceId . nextSliceId)
  sm  <- gets sliceMap
  let (m_old, sm') = Map.insertLookupWithKey (\_k _new old -> old) wle nsl sm
  case m_old of
    Nothing -> do
      modify (\s -> s { sliceMap = sm'
                      , worklist = Set.insert wle (worklist s)
                      , nextSliceId = nextSliceId s + 1 })
      pure nsl
    Just nsl' -> pure nsl'

nameExpSlice :: ExpSlice -> Maybe SliceId -> ExpM ae SliceId
nameExpSlice sl m_sid = do
  sid <- case m_sid of
           Nothing  -> SliceId <$> (field @"nextSliceId" <<+= 1)
           Just sid -> pure sid
  field @"slices" . at sid .= Just sl
  pure sid
  
refreshName :: Name -> ExpM ae Name
refreshName n = do
  seen <- gets (Set.member n . seenNames)
  if seen
    then freshName n
    else do
    modify (\s -> s { seenNames = Set.insert n (seenNames s) })
    pure n

substNameIn :: Name -> Name -> ExpM ae a -> ExpM ae a
substNameIn n n' = local (Map.insert n n')

--------------------------------------------------------------------------------
-- Worker code

type Worklist = Set (FName, FInstId, Set Int)

exportSummaries :: Map TName TDecl -> (Summaries ae, GUID) -> (ExpSummaries, GUID)
exportSummaries tenv (summs, nguid) = (expSumms, nextGUID st')
  where
    expSumms = ExpSummaries
      { esRootSlices     = roots
      , esFunctionSlices = slices st'
      , esRecs           = recs
      -- These rely on root slices nnot being in SCCs
      , esRecVars        = makeEsRecVars   (slices st') recs
      , esBackEdges      = makeEsBackEdges (slices st') recs
      }

    recs = makeEsRecs (slices st')

    (roots, st') = runExpM go st0
    st0 = ExpState { seenNames      = mempty
                   , worklist       = mempty
                   , sliceMap       = mempty
                   , slices         = mempty
                   , nextGUID       = nguid
                   , nextSliceId    = 0
                   , summaries      = summs
                   , tyEnv          = tenv
                   }

    go = do
      -- THe internal elements are the entry points for synthesis, and
      -- do not call each other, so we start here.  We initially only
      -- need the assertionFID slices, but this is simpler (we could
      -- start at the assertionFID and extend when we see a FInstId
      -- that we haven't seen before).
      rs <- traverse (traverse (goDom . domain)) summs
      modify goWL
      pure rs

    goWL s@ExpState { worklist = wl } | Just (wle, wl') <- Set.minView wl
      = let s' = s { worklist = wl' }
        in goWL . snd $ runExpM (goFunSlice wle) s'
    goWL st = st

    goFunSlice wle@(fn, fid, ixs) = do
      Summary { domain = ds } <- getSummary fn fid
      m_sid <- gets (Map.lookup wle . sliceMap)
      void $ exportSlice m_sid (foldl1 merge [ gsSlice gs | (i, gs) <- zip [0..] (elements ds), i `Set.member` ixs ])
    
    goDom :: forall ae. Domain ae -> ExpM ae ExpSummary
    goDom d = traverse (mapM (exportSlice Nothing)) (closedElements d)

exportSlice :: Maybe SliceId -> Slice -> ExpM ae SliceId
exportSlice m_sid sl0 = do
  esl <- go sl0
  nameExpSlice esl m_sid
  where
    go sl =
      case sl of
        SHole     -> pure SHole
        SPure sle -> SPure <$> goE sle

        SDo x l r -> do
          x' <- refreshName x
          SDo x' <$> go l <*> substNameIn x x' (go r)

        SMatch e   -> SMatch <$> doSubst e
        SChoice cs -> SChoice <$> traverse go cs
        SCall cn   -> SCall <$> exportCallNode cn
        SCase t cs -> do
          Case x cs' <- traverse go cs
          x' <- goN x
          pure (SCase t (Case x' cs'))

        SLoop str lcl -> SLoop str <$>
          case lcl of 
            ManyLoop sem bt lb m_ub g ->
              ManyLoop sem bt <$> goE lb
                              <*> traverse goE m_ub
                              <*> go g
            RepeatLoop bt n e b -> do
              n' <- refreshName n
              RepeatLoop bt n' <$> goE e <*> substNameIn n n' (go b)
            MorphismLoop (FoldMorphism n e lc b) -> do 
              n' <- refreshName n
              MorphismLoop <$> goLC lc (FoldMorphism n <$> goE e) (substNameIn n n' (go b))
            MorphismLoop (MapMorphism lc b) ->
              MorphismLoop <$> goLC lc (pure MapMorphism) (go b)
            
        SInverse n f p -> do
          n' <- refreshName n
          substNameIn n n' (SInverse n' <$> doSubst f <*> doSubst p)

    goN :: Name -> ExpM ae Name
    goN n = asks (Map.findWithDefault n n)

    goE sle = do
      tenv <- gets tyEnv
      doSubst (slExprToExpr' tenv sle)

    goLC lc mk bm = do
      eln'  <- refreshName (lcElName lc)
      m_kn' <- traverse refreshName (lcKName lc)
      mk <*> (LoopCollection m_kn' eln' <$> goE (lcCol lc))
         <*> substNameIn (lcElName lc) eln' 
               (maybe id (uncurry substNameIn) ((,) <$> lcKName lc <*> m_kn')
               bm)

    doSubst :: Subst e => e -> ExpM ae e
    doSubst e = do
      subst <- asks (fmap Var)
      substitute subst e


exportCallNode :: CallNode -> ExpM ae ExpCallNode
exportCallNode cn = do
  subst <- ask
  Summary { params = ps } <- getSummary (callName cn) (callClass cn)
  let ixs = Map.keysSet (callSlices cn)
      -- Collect param map
      mk  = Map.fromList . catMaybes . zipWith (fmap . (,)) ps
      doSubst n = Map.findWithDefault n n subst
      paramMap = doSubst <$> foldMap mk (callSlices cn)

  sid <- getAllocSliceId (callName cn, callClass cn, ixs)
  pure (ExpCallNode { ecnName = callName cn
                    , ecnIdx  = callClass cn
                    , ecnSliceId  = sid
                    , ecnParamMap = paramMap
                    })

--------------------------------------------------------------------------------
-- Instances

instance TraverseUserTypes ExpCallNode where
  traverseUserTypes f cn  =
    (\n' pm' -> cn { ecnName = n', ecnParamMap = pm' })
       <$> traverseUserTypes f (ecnName cn)
       <*> (Map.fromList <$> traverse goPair (Map.toList (ecnParamMap cn)))
    where
      goPair (n, n') = (,) <$> traverseUserTypes f n <*> traverseUserTypes f n'

instance FreeVars ExpCallNode where
  -- We don't include the ran of the paramMap as they refer to vars in
  -- the target.
  freeVars cn  = foldMap freeVars (Map.elems (ecnParamMap cn))
  freeFVars cn = Set.singleton (ecnName cn)

-- Probably should only be used for debugging.
instance PP ExpCallNode where
  pp cn = pp (ecnName cn) <> "." <> pp (ecnIdx cn) <> "." <> pp (ecnSliceId cn) <> ppM (ecnParamMap cn)
    where
      ppM m = block "{" ", " "}" [ ppN k <> " -> " <> ppN v
                                 | (k,v) <- Map.toList m ]
      ppN n = ppPrec 1 n <> parens (pp (nameId n))



