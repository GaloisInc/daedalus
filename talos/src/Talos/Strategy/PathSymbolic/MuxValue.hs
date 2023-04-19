{-# LANGUAGE GADTs, PatternGuards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- -----------------------------------------------------------------------------
-- Semi symbolic/concrete evaluation
--
-- This is comied and modified from SemiExpr

module Talos.Strategy.PathSymbolic.MuxValue where

import           Control.Lens                              (locally)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe                 (MaybeT, runMaybeT)
import           Data.Generics.Product                     (field)
import           Data.List                                 (transpose)
import           Data.List.NonEmpty                        (NonEmpty (..),
                                                            nonEmpty)
import qualified Data.List.NonEmpty                        as NE
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import           Data.Maybe                                (catMaybes, mapMaybe)
import           Data.Semigroup                            (sconcat)
import qualified Data.Set                                  as Set
import qualified Data.Vector                               as Vector
import           GHC.Generics                              (Generic)
import           GHC.Stack                                 (HasCallStack)

import           SimpleSMT                                 (SExpr)
import qualified SimpleSMT                                 as S

import           Daedalus.Core                             hiding (freshName)
import qualified Daedalus.Core.Semantics.Env               as I
import           Daedalus.Core.Semantics.Expr              (evalOp0, evalOp1,
                                                            evalOp2, matches,
                                                            partial)
import           Daedalus.Core.Type
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic
import qualified Daedalus.Value.Type                       as V

import           Talos.Strategy.PathSymbolic.PathCondition (LoopCountVar,
                                                            PathCondition (..),
                                                            ValuePathConstraint (..),
                                                            loopCountVarToSMTVar)
import qualified Talos.Strategy.PathSymbolic.PathCondition as PC
import qualified Talos.SymExec.Expr                        as SE
import           Talos.SymExec.SolverT
import           Talos.SymExec.StdLib
import           Talos.SymExec.Type
import Control.Monad.State (StateT, runStateT, modify)
import Data.Set (Set)

--------------------------------------------------------------------------------
-- GuardedValues

-- | A GuardedValues represents a value along a number of paths.

-- sjw: I think no two guards should be simultaneously satisfiable, as
-- tha would mean we would be on 2 paths at the same time.  This might
-- not be the case if we have duplicates, or the same valye with
-- different guards.
newtype GuardedValues a = GuardedValues {
  getGuardedValues :: NonEmpty (PathCondition, GuardedValue a)
  } deriving (Functor, Foldable, Traversable)

guardedValues :: GuardedValues a -> [(PathCondition, GuardedValue a)]
guardedValues = NE.toList . getGuardedValues
  
type GuardedValue a = MuxValue GuardedValues a

-- | This type is used to track where a sequence was generated, so
-- that we can link uses and defs for doing pool-generation of
-- sequenece elements.
type SequenceTag = GUID

data MuxValue f a =
    VValue                 !V.Value
  | VOther                 !a
  | VUnionElem             !V.Label !(f a)
  -- We don't need to support partial updates (e.g. x { foo = bar }
  -- where x is symbolic) as Daedalus doesn't (yet) support updates.
  | VStruct                ![(V.Label, f a)]

  -- The bool is used to tell us if this is an array or builder, use in toValue
  | VSequence              VSequenceMeta ![f a]
  | VJust                  !(f a)

  -- -- For iterators and maps
  -- | VPair                  !(f a) !(f a)
  
  -- We support symbolic keys, so we can't use Map here

  | VMap                   ![(f a, f a)]
  | VIterator              ![(f a, f a)]
    deriving (Show, Eq, Ord, Foldable, Traversable, Functor)

data VSequenceMeta = VSequenceMeta
  { vsmGeneratorTag :: Maybe SequenceTag
  , vsmLoopCountVar :: Maybe LoopCountVar
  , vsmMinLength    :: Int
  -- ^ This is the minimum length of the list, it allows us to not
  -- generate cases which are impossible.
  , vsmIsBuilder    :: Bool
  } deriving (Show, Eq, Ord, Generic)

emptyVSequenceMeta :: VSequenceMeta
emptyVSequenceMeta = VSequenceMeta
  { vsmGeneratorTag = Nothing
  , vsmLoopCountVar = Nothing
  , vsmMinLength    = 0
  , vsmIsBuilder    = False
  }

-- Typed so we can turn into a value.
type GuardedSemiSExprs = GuardedValues (Typed SMTVar)
type GuardedSemiSExpr  = GuardedValue  (Typed SMTVar)

singleton :: PathCondition -> GuardedSemiSExpr -> GuardedSemiSExprs
singleton g gv = GuardedValues ((g, gv) :| [])

muxValueToList :: (V.Value -> f a) -> MuxValue f a -> Maybe (VSequenceMeta, [f a])
muxValueToList mk mv =
  case mv of
    VSequence m xs       -> Just (m, xs)
    VValue (V.VArray v)    -> fromValue False (map mk (Vector.toList v))
    -- Builders are stored in reverse order.
    VValue (V.VBuilder vs) -> fromValue True (map mk (reverse vs))
    _ -> Nothing

  where
    -- We don't weed out ArrayL and builder ops, so it is possible for
    -- interp arrays to occur, so we need to construct a vsequence meta)
    fromValue b vs =
      Just ( emptyVSequenceMeta { vsmMinLength = length vs, vsmIsBuilder = b }
           , vs)

gseToList :: GuardedSemiSExpr -> Maybe (VSequenceMeta, [GuardedSemiSExprs])
gseToList = muxValueToList (vValue mempty)

-- -----------------------------------------------------------------------------
-- Values

vUnit :: GuardedSemiSExprs
vUnit = singleton mempty $ VValue V.vUnit

vOther :: PathCondition -> Typed SMTVar -> GuardedSemiSExprs
vOther g = singleton g . VOther 

vSExpr :: (MonadIO m, HasGUID m) =>
          PathCondition -> Type -> SExpr -> SemiSolverM m GuardedSemiSExprs
vSExpr g ty se = vOther g . Typed ty <$> sexprAsSMTVar ty se

vValue :: PathCondition -> V.Value -> GuardedSemiSExprs
vValue g = singleton g . VValue

vBool :: PathCondition -> Bool -> GuardedSemiSExprs
vBool g = vValue g . V.VBool

vUInt :: PathCondition -> Int -> Integer -> GuardedSemiSExprs
vUInt g n = vValue g . V.vUInt n

-- vFst :: GuardedSemiSExprs -> Maybe GuardedSemiSExprs
-- vFst gvs = sconcat <$> nonEmpty gvss
--   where
--     gvss = catMaybes [ refineGuardedSemiExprs g v
--                      | (g, (v, _) <- guardedValues gvs ]

-- vSnd :: GuardedSemiSExprs -> Maybe GuardedSemiSExprs
-- vSnd gvs = sconcat <$> nonEmpty gvss
--   where
--     gvss = catMaybes [ refineGuardedSemiExprs g v
--                      | (g, VPair _ v) <- guardedValues gvs ]

-- pattern VBool :: Bool -> SemiValue a
-- pattern VBool b = VValue (V.VBool b)

pattern VNothing :: MuxValue f a
pattern VNothing = VValue (V.VMaybe Nothing)

refine :: PathCondition -> GuardedSemiSExprs -> Maybe GuardedSemiSExprs
refine g gvs =
  GuardedValues <$> nonEmpty [ (gr, v)
                             | (g', v) <- guardedValues gvs
                             , let gr = g <> g'
                             , gr /= Infeasible
                             ]

unions :: NonEmpty GuardedSemiSExprs -> GuardedSemiSExprs
unions ((GuardedValues ((_, VValue V.VUnit) :| _)) :| _) = vUnit
unions xs = GuardedValues . sconcat . fmap getGuardedValues $ xs

unions' :: [GuardedSemiSExprs] -> Maybe GuardedSemiSExprs
unions' = fmap unions . nonEmpty

-- -----------------------------------------------------------------------------
-- Sequences
-- 

-- | Turns a sequence, which may be a super-position of multiple
-- sequence lengths, into multiple sequences where the length is
-- fixed.
explodeSequence :: VSequenceMeta -> [GuardedSemiSExprs]
                -> [ (PathCondition, [GuardedSemiSExprs]) ]
explodeSequence vsm els
  | Just lcv <- vsmLoopCountVar vsm = map (go lcv) [vsmMinLength vsm .. length els - 1] 
  | otherwise = [(mempty, els)]
  where
    -- FIXME: this is inefficient, maybe store loops in reversed order (to get sharing)
    go lcv len = (PC.insertLoopCount lcv (PC.LCCEq len) mempty, take len els)

-- -----------------------------------------------------------------------------
-- Byte sets
-- 
-- At the moment we just symbolically execute

-- Maybe we could do better here than just symbolically evaluating,
-- but given that the byte is usually symbolic, just sending it to the
-- solver seems to be OK.
-- semiExecByteSet :: (HasGUID m, Monad m, MonadIO m) => ByteSet -> SExpr ->
--                    SemiSolverM m GuardedSemiSExprs
-- semiExecByteSet byteSet b =
--   vSExpr mempty TBool <$> symExecToSemiExec (SE.symExecByteSet b byteSet) -- go byteSet
  -- where
  --   go bs = case bs of
  --     SetAny         -> pure (vBool mempty True)
  --     SetSingle e    -> semiExecEq byteT b =<<
  --                         (vSExpr mempty TBool =<< semiExecExpr e)
  --     SetRange el eh -> conj <$> (flip (op2 Leq) (vSExpr b) =<< semiExecExpr el)
  --                                 <*> (op2 Leq (vSExpr b) =<< semiExecExpr eh)
  --     SetComplement bs' -> op1 Neg TBool =<< go bs'
  --     SetLet n e bs' -> do
  --       ve  <- semiExecExpr e
  --       bindNameIn n ve (go bs')
  --     SetCall fn args -> do
  --       fdefs <- asks byteFunDefs
  --       let (ps, bs') = case Map.lookup fn fdefs of
  --             Just fdef | Def d <- fDef fdef -> (fParams fdef, d)
  --             _   -> panic "Missing function " [showPP fn]
  --       vs <- mapM semiExecExpr args
  --       foldr (uncurry bindNameIn) (go bs') (zip ps vs)
  --     -- This 
  --     SetCase _cs -> panic "Saw a case in ByteSet" []
  --       -- m_e <- semiExecCase cs
  --       -- case m_e of
  --       --   -- can't determine match, just return a sexpr
  --       --   TooSymbolic   -> symExec expr
  --       --   DidMatch _ e' -> go e'
  --       --   -- Shouldn't happen in pure code
  --       --   NoMatch -> panic "No match" []
        
  --     where
  --       op1 op = semiExecOp1 op TBool
  --       op2 op = semiExecOp2 op TBool (TUInt (TSize 8)) (TUInt (TSize 8))

-- -- -----------------------------------------------------------------------------
-- -- Fallin back to (fully) symbolic execution

symExecToSemiExec :: (HasGUID m, Monad m, MonadIO m) => SE.SymExecM m a -> SemiSolverM m a
symExecToSemiExec = lift . lift . withReaderT envf
  where
    envf env = envToSymEnv (typeDefs env) (localBoundNames env)

-- symExec :: (HasGUID m, Monad m, MonadIO m) => Expr -> SemiSolverM m SemiSExpr
-- symExec e = vSExpr (typeOf e) <$> symExecToSemiExec (SE.symExecExpr e)

symExecByteSet :: (HasGUID m, Monad m, MonadIO m) => ByteSet -> SExpr -> SemiSolverM m GuardedSemiSExprs
symExecByteSet bs b = do
  se <- symExecToSemiExec (SE.symExecByteSet b bs)
  vSExpr mempty TBool se

envToSymEnv :: Map TName TDecl -> Map Name GuardedSemiSExprs ->  Map Name SExpr
envToSymEnv tenv = Map.mapWithKey (toSExpr tenv . nameType)

-- ASSUME: one guard is true, so the negation of all but one entails
-- the remaining guard.
toSExpr :: Map TName TDecl -> Type ->
           GuardedSemiSExprs -> SExpr
toSExpr tys ty gvs =
  foldr (\(g', el') -> S.ite (PC.toSExpr g') (go el')) (go el) els
  where
    go = toSExpr1 tys ty
    (_g, el) :| els = getGuardedValues gvs

toSExpr1 :: Map TName TDecl -> Type ->
            GuardedSemiSExpr -> SExpr
toSExpr1 tys ty sv = 
  case sv of
    VValue v -> valueToSExpr tys ty v
    VOther s -> S.const (typedThing s)
    -- FIXME: copied from valueToSExpr, unify.
    VUnionElem l v' | Just (ut, ty') <- typeAtLabel tys ty l
      -> S.fun (labelToField (utName ut) l) [go ty' v']

    -- FIXME: copied from valueToSExpr, unify.
    VStruct els
      | TUser ut <- ty
      , Just TDecl { tDef = TStruct flds } <- Map.lookup (utName ut) tys
      -> S.fun (typeNameToCtor (utName ut)) (zipWith goStruct els flds)

    -- Should probably never happen?
    VSequence _ _ -> panic "Saw toSExpr1 on VSequence" []
      --  | Just elTy <- typeToElType ty ->
      --      sArrayLit (symExecTy elTy) (typeDefault elTy) (map (go elTy) vs)
          
    VJust v' | TMaybe ty' <- ty -> sJust (symExecTy ty') (go ty' v')

    VMap ms | TMap kt vt <- ty ->
      -- FIXME: breaks abstraction of maps
      sFromList (tTuple (symExecTy kt) (symExecTy vt))
                [ sTuple (go kt k) (go vt v) | (k, v) <- ms ]

    VIterator vs -> case ty of
      -- FIXME: this breaks abstractions
      TIterator (TArray elTy) ->
        let emptyA = sEmptyL (symExecTy elTy) (typeDefault elTy)
            els    = [ (go sizeType k, go elTy v) | (k, v) <- vs ]
            arr    = foldr (\(i, v) arr' -> S.store arr' i v) emptyA els
        in case els of
          []             -> sArrayIterNew emptyA
          ((fstI, _) : _) -> S.fun "mk-ArrayIter" [arr, fstI]
          
      TIterator (TMap   _kt' _vt') -> panic "Unimplemented" []
      _ -> panic "Malformed iterator type" []
      
    _ -> panic "Malformed value" [showPP ty]
  where
    go = toSExpr tys
    goStruct (l, e) (l', ty') | l == l' = go ty' e
    goStruct (l, _) (l', _) = panic "Mis-matched labels" [showPP l, showPP l']

semiExecName :: (HasGUID m, Monad m, HasCallStack) =>
                Name -> SemiSolverM m GuardedSemiSExprs
semiExecName n = do
  m_local <- asks (Map.lookup n . localBoundNames)
  case m_local of
    Nothing -> panic "Missing name" [showPP n]
    Just r  -> pure r

bindNameIn :: Monad m => Name -> GuardedSemiSExprs ->
              SemiSolverM m a -> SemiSolverM m a
bindNameIn n v =
  locally (field @"localBoundNames") (Map.insert n v)

-- -- Stolen from Synthesis
-- -- projectEnvFor :: FreeVars t => t -> I.Env -> SynthEnv -> Maybe I.Env
-- -- projectEnvFor tm env0 se = doMerge <$> Map.traverseMaybeWithKey go (synthValueEnv se)
-- --   where
-- --     frees = freeVars tm

-- --     doMerge m = env0 { I.vEnv = Map.union m (I.vEnv env0) } 

-- --     go k v | k `Set.member` frees = Just <$> projectInterpValue v
-- --     go _ _                        = Just Nothing

-- -- projectEnvForM :: FreeVars t => t -> SynthesisM I.Env
-- -- projectEnvForM tm = do
-- --   env0 <- getIEnv
-- --   m_e <- SynthesisM $ asks (projectEnvFor tm env0)
-- --   case m_e of
-- --     Just e  -> pure e
-- --     Nothing -> panic "Captured stream value" []

-- Stolen from Daedalus.Core.Semantics.Expr.  Returns Nothing if the
-- pattern will naver match, otherwise Just (sideConds)
matches' :: GuardedSemiSExpr -> Pattern -> Bool
matches' v pat =
  case (pat, v) of
    (PAny, _) -> found
    (_, VValue v') -> matches pat v'
    (_, VOther {}) -> panic "Saw VOther" []
    (PNothing, VNothing) -> found
    (PJust, VJust {})    -> found
    (PCon l, VUnionElem l' _) -> l == l'
    _ -> False
  where
    found = True
    
-- Case is a bit tricky
--
-- In general, if we have values (g1, v1) ... (gn, vn) and patterns
-- p1, ..., pM, then a symbolic vk may need to be run on all alts,
-- with the constraint that the pattern matches the value.  We also
-- require both that some value is reachable, and that 
--
-- For each pattern we generate a refined value (may not be needed,
-- but might be more efficient and help in debugging) and a path
-- condition extension.
--
-- For example, consider the following case
--
--   case x of
--     1 -> r1
--     2 -> r2
--
-- Given x is the singleton [ (g1, VOther n) ] we generate the path condition
-- (n = 1) for r1, (n = 2) for r2, and
--
--      (g1 --> (n = 1 \/ n = 2))
--
-- which says if the value is reachable, then one branch must be true.
-- We also assert g1 to say that some value must be reachable (is this required?)
-- Finally, we update x to be (g1 /\ n = 1, VValue 1) in r1 and
-- (g1 /\ n = 2, VValue 2) in r2.
--
--
-- For the case of multiple values, say
--
--  [ (g1, VOther n), (g2, VValue 1), (g3, VValue 42) ]
--
-- we have for
--  r1: x = [ (g1 /\ n = 1, VOther 1), (g2, VValue 2) ]
--  r2: x = [ (g1 /\ n = 2, VOther 2) ]
--
-- (g1 --> (n = 1 \/ n = 2)) /\ (g2 --> True) /\ (g3 --> False)

-- ====================
-- x = First
--   inl = {}
--   inr = {}
-- case x of
--   inl -> {}
--
-- x = [ (c = 0, inl), (c = 1, inr) ]

-- ====================
-- x = ^1 | ^2 -- x |-> [ (c = 0, 1), (c = 1, 2) ]
-- y = ^ x -- x |-> [ (c = 0, 1), (c = 1, 2) ], y |-> [ (c = 0, 1), (c = 1, 2) ]
-- 
-- case x of
--   1 -> { case (y = 1) of true -> {} }

-- ====================
-- x = UInt8 -- x |-> [ (True, VOther n) ]
-- y = ^ x   -- x |-> [ (True, VOther n) ]; y |-> [ (True, VOther n) ]
-- case x of
--   1 -> -- PC = { n = 1 }
--        { let p = (y = 1) -- p |-> [ ( True, VOther (n = 1) ) ]
--        ; case p of true -> {}
--        -- n = 1 = (True /\ n = 1)
--        }
-- (True --> n = 1)

-- ==================== (data dep)
-- x = First
--   A = {}
--   B = {}
--   C = {}
-- y = case x of
--   A -> 1
--   B -> 2
--   C -> 1
-- z = (y = 1) -- z |-> [ (c = 0, true), (c = 1, false), (c = 2, true) ]
-- case z of true -> {}
-- either: (c = 0 /\ true) \/ )c = 1 /\ false) \/ (c = 2 /\ true)
-- or: (c = 0 --> true) /\ (c = 1 --> false) /\ (c = 2 --> true)

-- x = [ (c = 0, inl), (c = 1, inr) ]


-- ASSUME: values in env are non-empty
--

-- | This function calculates which RHSs of the case are enabled by
-- the values for the target variable.  For each RHS which is enabled,
-- this function returns the valueguards for the values which match
-- the pattern.  These valueguards are those for the values, extended
-- with predicates claiming that the value matches the pattern; this
-- only matters for symbolic values, where we assert that the symbolic
-- value matches the pattern, or, for the case of the default pattern,
-- that the value _doesn't_ match the other patterns.
--
-- The second result is a list of (valueguard, predicates) which are
-- used to ensure that some value matches a pattern, and that values
-- which do not match any pattern are discarded.

-- FIXME: we could also refine the targeted value, but it may not help
-- much (we could e.g. throw away the non-matching values and then
-- merge the environment at each join point, which might make the code
-- more efficient?)

data ValueMatchResult = NoMatch | YesMatch | SymbolicMatch SExpr
  deriving (Eq, Ord, Show)

semiExecCase :: (Monad m, HasGUID m, HasCallStack) =>
                Case a ->
                SemiSolverM m ( [ ( NonEmpty PathCondition, (Pattern, a)) ]
                              , [ (PathCondition, ValueMatchResult) ] )
semiExecCase (Case y pats) = do
  els <- guardedValues <$> semiExecName y
  let (vgs, preds) = unzip (map goV els)
      vgs_for_pats = map (nonEmpty . filter PC.isFeasibleMaybe) (transpose vgs)
      allRes       = zipWith (\a -> fmap (, a)) pats vgs_for_pats
  pure (catMaybes allRes, preds)
  where
    -- This function returns, for a given value, a path-condition for
    -- each pattern stating when that PC is enabled, and a predicate
    -- for when some pattern matched.
    goV :: (PathCondition, GuardedSemiSExpr) ->
           ( [ PathCondition ], (PathCondition, ValueMatchResult) )
    -- Symbolic case
    goV (g, VOther x) =
      let basePreds = map (\p -> SE.patternToPredicate ty p (S.const (typedThing x))) pats'
          (m_any, assn)
            -- if we have a default case, the constraint is that none
            -- of the above matched
            | hasAny    = ([ VPCNegative (Set.fromList pats') ], YesMatch)
            | otherwise = ([], SymbolicMatch (orMany basePreds))
          vgciFor c = Typed ty c
      in (map (\c -> PC.insertValue (typedThing x) (vgciFor c) g) (map VPCPositive pats' ++ m_any), (g, assn))
    goV (g, v) =
      let ms = map (matches' v) pats'
          noMatch = not (or ms)
          (m_any, assn)
            | hasAny && noMatch = ( [True], YesMatch )
            | hasAny            = ( [False], YesMatch )
            | noMatch           = ( [], NoMatch )
            -- FIXME: do we need the (g, YesMatch) here?
            | otherwise         = ( [], YesMatch )
      in ( [ if b then g else Infeasible | b <- ms ++ m_any], (g, assn) )

    -- ASSUME that a PAny is the last element
    pats'  = [ p | p <- map fst pats, p /= PAny ]
    hasAny = PAny `elem` map fst pats
    
    ty = typeOf y

-- -- -----------------------------------------------------------------------------
-- -- Monad

data SemiSolverEnv = SemiSolverEnv
  { localBoundNames :: Map Name GuardedSemiSExprs
  -- for concretely evaluating functions, only const/pure fun env
  -- should be used.
  , interpEnv       :: I.Env
  } deriving (Generic)


typeDefs :: SemiSolverEnv -> Map TName TDecl
typeDefs = I.tEnv . interpEnv

type SemiCtxt m = (Monad m, MonadIO m, HasGUID m)

type SemiState = Set (Typed SMTVar)

type SemiSolverM m = MaybeT (StateT SemiState (ReaderT SemiSolverEnv (SolverT m)))

runSemiSolverM :: SemiCtxt m =>
                  Map Name GuardedSemiSExprs ->
                  I.Env ->
                  SemiSolverM m a -> SolverT m (Maybe a, Set (Typed SMTVar))
runSemiSolverM lenv env m = 
  runReaderT (runStateT (runMaybeT m) mempty) (SemiSolverEnv lenv env)

sexprAsSMTVar :: (MonadIO m, HasGUID m) => Type -> SExpr -> SemiSolverM m SMTVar
sexprAsSMTVar _ty (S.Atom x) = pure x
sexprAsSMTVar ty e = do
  s <- liftSolver (defineSymbol "named" (symExecTy ty) e)
  modify (Set.insert (Typed ty s))
  pure s

getMaybe :: SemiCtxt m => SemiSolverM m a -> SemiSolverM m (Maybe a)
getMaybe = lift . runMaybeT 

putMaybe :: SemiCtxt m => SemiSolverM m (Maybe a) -> SemiSolverM m a
putMaybe m = hoistMaybe =<< m

hoistMaybe :: SemiCtxt m => Maybe a -> SemiSolverM m a
hoistMaybe r = 
  case r of
    Nothing -> fail "Ignored"
    Just v  -> pure v

collectMaybes :: SemiCtxt m => [SemiSolverM m a] -> SemiSolverM m [a]
collectMaybes = fmap catMaybes . mapM getMaybe

gseCollect :: SemiCtxt m => [SemiSolverM m GuardedSemiSExprs] ->
              SemiSolverM m GuardedSemiSExprs
gseCollect gvs = collectMaybes gvs >>= hoistMaybe . unions'



--------------------------------------------------------------------------------
-- Exprs

semiExecExpr :: (SemiCtxt m, HasCallStack) =>
                Expr -> SemiSolverM m GuardedSemiSExprs
semiExecExpr expr = 
  case expr of
    Var n          -> semiExecName n
    PureLet n e e' -> do
      ve  <- semiExecExpr e
      bindNameIn n ve (semiExecExpr e') -- Maybe we should generate a let?

    Struct _ut ctors -> do
      let (ls, es) = unzip ctors
      sves <- mapM semiExecExpr es
      pure (singleton mempty $ VStruct (zip ls sves))

    ECase cs -> do
      (ms, _partialPred) <- semiExecCase cs
      let mk1 (gs, (_pat, e)) = do
            v <- semiExecExpr e
            pure $ mapMaybe (\g -> refine g v) (NE.toList gs)
      els <- concat <$> mapM mk1 ms
      hoistMaybe (unions' els)

    -- These should be lifted in LiftExpr
    ELoop _lm    -> panic "Impossible" []
        
    Ap0 op       -> pure (vValue mempty $ partial (evalOp0 op))
    Ap1 op e     -> semiExecOp1 op rty (typeOf e) =<< semiExecExpr e
    Ap2 op e1 e2 ->
      join (semiExecOp2 op rty (typeOf e1) (typeOf e2)
            <$> semiExecExpr e1
            <*> semiExecExpr e2)
    Ap3 op e1 e2 e3 ->
      join (semiExecOp3 op rty (typeOf e1)
             <$> semiExecExpr e1
             <*> semiExecExpr e2
             <*> semiExecExpr e3)
    ApN opN vs     -> semiExecOpN opN rty =<< mapM semiExecExpr vs
  where
    rty = typeOf expr

semiExecOp1 :: SemiCtxt m => Op1 -> Type -> Type ->
               GuardedSemiSExprs ->
               SemiSolverM m GuardedSemiSExprs
semiExecOp1 EJust _rty _ty gvs =
  pure . singleton mempty $ VJust gvs

semiExecOp1 (InUnion _ut l)  _rty _ty sv =
  pure . singleton mempty $ VUnionElem l sv

-- We do this component-wise
semiExecOp1 op rty ty gvs = gseCollect (map go (guardedValues gvs))
  where
    mk1 g = pure . singleton g
    
    -- This function executes a for a single value, returning Nothing
    -- if the value is of the wrong shape (e.g. not Just).  It is the
    -- responsibility of the context to make sure that the right
    -- assertions are in place so that ignoring results is OK.  In
    -- practice, this means that partial operations are guarded by a
    -- case.
    go (g, VOther x) =
      vSExpr g rty =<< liftSolver (SE.symExecOp1 op ty (S.const (typedThing x)))
  
    -- Value has a concrete head
    go (g, sv) = case op of
      IsEmptyStream -> unimplemented
      Head          -> unimplemented
      StreamOffset  -> unimplemented
      StreamLen     -> unimplemented
      ArrayLen
        | Just (vsm, _svs) <- gseToList sv
        , Just lcv <- vsmLoopCountVar vsm -> mk1 g $ VOther (Typed sizeType (loopCountVarToSMTVar lcv))
        -- Otherwise we just use the length.  This should be the non-many generated case.
        | Just (_, svs) <- gseToList sv -> mk1 g $ VValue (V.vSize (toInteger (length svs)))
        | otherwise -> panic "BUG: saw non-concrete list" []
        
      Concat
        | Just (vsm, svs)  <- gseToList sv -> hoistMaybe (refine g =<< gseConcat vsm svs)
        | otherwise -> panic "BUG: saw non-concrete list" []
        
      -- Just flip the 'isbuilder' flag
      FinishBuilder
        | VSequence vsm els <- sv ->
          mk1 g (VSequence (vsm { vsmIsBuilder = False}) els)
          
      -- NewIterator
      --   | TArray {} <- ty
      --   , Just svs  <- toL sv ->
      --     let ixs = map (vValue mempty . V.vSize) [0..]
      --         els = zip ixs svs
      --     in mk1 g $ VIterator els

      -- Shouldn't really happen as loops take care of iterators,
      -- although they can still be used directly.
      NewIterator -> unimplemented
      IteratorDone -> unimplemented
      IteratorKey  -> unimplemented
      IteratorNext -> unimplemented
      
      -- IteratorDone  | VIterator els <- sv -> pure (vBool g (null els))
      -- IteratorKey
      --   | VIterator ((k, _) : _) <- sv  ->
      --       hoistMaybe (refine g k) -- FIXME: correct?
      --   | VIterator [] <- sv -> hoistMaybe Nothing
        
      -- IteratorVal
      --   | VIterator ((_, v) : _) <- sv ->
      --       hoistMaybe (refine g v) -- FIXME: correct?
      --   | VIterator [] <- sv -> hoistMaybe Nothing 
            
      -- IteratorNext
      --   | VIterator (_ : els) <- sv -> mk1 g (VIterator els)
      --   | VIterator [] <- sv -> hoistMaybe Nothing


      -- handled above.
      -- EJust -> mk1 g (VJust sv)
      FromJust
        | VJust sv' <- sv -> hoistMaybe (refine g sv')
        | VNothing <- sv  -> hoistMaybe Nothing
  
      SelStruct _ty l
        | VStruct flds <- sv
        , Just sv' <- lookup l flds -> hoistMaybe (refine g sv')
        | VStruct _flds <- sv -> panic "Missing field" [showPP l]
  
      -- InUnion _ut l                     -> pure (VUnionElem l sv)
      FromUnion _ty l
        -- FIXME: we should have already dealt with g in the
        -- surrounding case, so we can discard g here (?)
        | VUnionElem l' sv' <- sv, l == l' -> pure sv'
        | VUnionElem {} <- sv -> hoistMaybe Nothing
        -- The other case is handled below for values
        | VValue (V.VUnionElem l' _) <- sv, l /= l' -> hoistMaybe Nothing

      -- Otherwise, if we have a value just call the evaluator
      _ | VValue v <- sv -> do
            env <- asks interpEnv
            mk1 g $ VValue (evalOp1 (I.tEnv env) op ty v)

      -- These are the remaining cases, which are only defined on values or sexprs
      --  CoerceTo Type
      --  BytesOfStream
      --  OneOf ByteString
      --  Neg
      --  BitNot
      --  Not
      --  WordToFloat
      --  WordToDouble
      --  IsNaN
      --  IsInfinite
      --  IsDenormalized
      --  IsNegativeZero
      _ -> panic "Uncaught case" [showPP op]
    
    unimplemented = panic "semiEvalOp1: Unimplemented" [showPP op]

-- | Concatentates a list of list-valued exprs
gseConcat :: VSequenceMeta -> [GuardedSemiSExprs] -> Maybe GuardedSemiSExprs
gseConcat _vsm _svs = panic "UNIMPLEMENTED: Concat" []

  -- unions' (mapMaybe mkOne combs)
  -- where
  --   elss = map guardedValues svs
  --   -- This is a sneaky way to get the list of products of the
  --   -- members.  This is potentially very expensive.
  --   combs = sequence elss

  --   doConcat els g'
  --     | PC.isInfeasible g' = Nothing    
  --     | Just els' <- mapM toL els = Just $ singleton g' (VSequence emptyVSequenceMeta (concat els'))
  --     | otherwise  = panic "UNIMPLEMENTED: saw symbolic list" []
    
  --   mkOne comb =
  --     let (gs, els) = unzip comb
  --     in doConcat els (mconcat gs)

typeToElType :: Type -> Maybe Type
typeToElType ty = 
  case ty of
    TBuilder elTy -> Just elTy
    TArray   elTy -> Just elTy
    _ -> Nothing

-- Short circuiting op
-- scBinOp :: MonadReader SemiSolverEnv m =>
--   (SExpr -> SExpr -> SExpr) ->
--   (SemiSExpr -> SemiSExpr) ->
--   (SemiSExpr -> SemiSExpr) ->
--   SemiSExpr -> SemiSExpr -> m SemiSExpr
-- scBinOp op tc fc x y =
--   case (x, y) of
--     (VBool True, _)  -> pure (tc y)
--     (VBool False, _) -> pure (fc y)
--     (_, VBool True)  -> pure (tc x)
--     (_, VBool False) -> pure (fc x)
--     _ -> do
--       tys <- asks typeDefs
--       pure (vSExpr TBool (op (semiSExprToSExpr tys TBool x)
--                              (semiSExprToSExpr tys TBool y)))

-- bAnd, bOr :: MonadReader SemiSolverEnv m => SemiSExpr -> SemiSExpr -> m SemiSExpr
-- bAnd = scBinOp S.and id (const (VBool False))
-- bOr  = scBinOp S.or (const (VBool True)) id

bOpMany :: SemiCtxt m => Bool -> PathCondition -> 
           [GuardedSemiSExprs] ->
           SemiSolverM m GuardedSemiSExprs
bOpMany opUnit g svs = gseCollect (map mkOne combs)
  where
    elss = map guardedValues svs
    -- This is a sneaky way to get the list of products of the
    -- members.  This is potentially very expensive.
    combs = sequence elss
    
    mkOne (unzip -> (gs, els)) = 
      go els (mconcat (g : gs))

    go els g'
      | PC.isInfeasible g' = fail "Ignored"
      | any (isBool absorb) els = mkB g' absorb
      | otherwise = do
          tys <- asks typeDefs
          -- strip out units and convert to sexpr
          let nonUnits = [ toSExpr1 tys TBool sv
                         | sv <- els, not (isBool opUnit sv) ]
          case nonUnits of
            [] -> mkB g' opUnit
            [el] -> vSExpr g' TBool el
            _    -> vSExpr g' TBool (op nonUnits)

    mkB g' = pure . vBool g'

    isBool b (VValue (V.VBool b')) = b == b'
    isBool _ _ = False
    op = if opUnit then andMany else orMany
    absorb = not opUnit

bAndMany, bOrMany :: SemiCtxt m => PathCondition -> 
                     [GuardedSemiSExprs] ->
                     SemiSolverM m GuardedSemiSExprs
bAndMany = bOpMany True
bOrMany  = bOpMany False

semiExecEq, semiExecNEq :: SemiCtxt m => Type ->
                           GuardedSemiSExprs -> GuardedSemiSExprs ->
                           SemiSolverM m GuardedSemiSExprs
semiExecEq  = semiExecEqNeq True
semiExecNEq = semiExecEqNeq False

semiExecEqNeq :: SemiCtxt m => Bool -> Type ->
                 GuardedSemiSExprs ->
                 GuardedSemiSExprs ->
                 SemiSolverM m GuardedSemiSExprs
semiExecEqNeq iseq ty gvs1 gvs2 =
  gseCollect [ go g sv1 sv2
             | (g1, sv1) <- guardedValues gvs1
             , (g2, sv2) <- guardedValues gvs2
             , let g = g1 <> g2, PC.isFeasibleMaybe g
             ]
  where
    go g sv1 sv2 = do
      let mkB = pure . vBool g
          
      tys <- asks typeDefs
      
      case (sv1, sv2) of
        (VValue v1, VValue v2) -> mkB (v1 `eqcmp` v2)
        (VUnionElem l sv1', VUnionElem l' sv2')
          | l == l', Just (_, ty') <- typeAtLabel tys ty l ->
              semiExecEqNeq iseq ty' sv1' sv2'
          | l == l'    -> panic "Missing label" [showPP l]
          | otherwise -> mkB (not iseq)
    
        (VStruct flds1, VStruct flds2) ->
          opMany g =<< zipWithM (go' tys) flds1 flds2
          
        (VJust sv1', VJust sv2')
          | TMaybe ty' <- ty -> semiExecEqNeq iseq ty' sv1' sv2'
        (VJust {}, VNothing) -> mkB (not iseq)
        (VNothing, VJust {}) -> mkB (not iseq)

        -- List/sequence equality.  We don't have the ability to just
        -- assert lengths are equal, so we have to add as a path
        -- condition.
        _ | Just (vsm1, svs1) <- gseToList sv1
          , Just (vsm2, svs2) <- gseToList sv2
          , Just elTy <- typeToElType ty ->
            let go1 g' svs1' svs2'            
                  | length svs1' /= length svs2' = pure (vBool g' (not iseq))
                  | otherwise = opMany g' =<< zipWithM (semiExecEqNeq iseq elTy) svs1' svs2'
            in gseCollect [ go1 g' svs1' svs2'
                          | (g1, svs1') <- explodeSequence vsm1 svs1
                          , (g2, svs2') <- explodeSequence vsm2 svs2
                          , let g' = g <> g1 <> g2, PC.isFeasibleMaybe g'
                          ]            
        _ -> do
          se <- liftSolver (SE.symExecOp2 op TBool
                            (toSExpr1 tys ty sv1)
                            (toSExpr1 tys ty sv2))
          vSExpr g TBool se
            
    (op, eqcmp, opMany) =
      if iseq
      then (Eq   , (==), bAndMany)
      else (NotEq, (/=), bOrMany)
    
    go' tys (l, sv1') (l', sv2') =
      if l == l'
      then case typeAtLabel tys ty l of
             Just (_, ty') -> semiExecEqNeq iseq ty' sv1' sv2'
             _             -> panic "Missing label" [showPP l]
      else panic "Label mismatch" [showPP l, showPP l']
    
semiExecOp2 :: SemiCtxt m => Op2 -> Type -> Type -> Type ->
               GuardedSemiSExprs -> GuardedSemiSExprs -> SemiSolverM m GuardedSemiSExprs
-- semiExecOp2 op _rty _ty _ty' (VValue v1) (VValue v2) = pure $ VValue (evalOp2 op v1 v2)
-- semiExecOp2 op rty   ty _ty' (VOther v1) (VOther v2) =
--   lift (vSExpr rty <$> SE.symExecOp2 op ty (typedThing v1) (typedThing v2))
semiExecOp2 Eq    _rty ty1 _ty2 sv1 sv2 = semiExecEq  ty1 sv1 sv2
semiExecOp2 NotEq _rty ty1 _ty2 sv1 sv2 = semiExecNEq ty1 sv1 sv2

-- If all the vals in sv1 are spine-concrete (which is the usual case) we can just append.
semiExecOp2 Emit _rty _ty1 _ty2 gvs1 gvs2
  | Just vss1 <- traverse gseToList gvss1 =
      -- FIXME: this breaks the abstraction a little
      pure (GuardedValues (NE.zipWith mk gs vss1))
  where
    mk g (vsm, vs) = (g, VSequence (vsm {vsmIsBuilder = True}) (vs ++ [gvs2]))
    (gs, gvss1) = NE.unzip (getGuardedValues gvs1)
    
semiExecOp2 op rty ty1 ty2 gvs1 gvs2 =
  gseCollect [ go g sv1 sv2
             | (g1, sv1) <- guardedValues gvs1
             , (g2, sv2) <- guardedValues gvs2
             , let g = g1 <> g2, PC.isFeasibleMaybe g
             ]
  where
    -- point-wise
    go g (VValue v1) (VValue v2) =
      pure (singleton g (VValue (evalOp2 op v1 v2)))

    -- Some of these are lazy so we don't eagerly convert to sexpr if
    -- only one side is a sexpr
    go g sv1 sv2 = do
      case op of
        IsPrefix -> unimplemented
        Drop     -> unimplemented
        Take     -> unimplemented

        -- Eq       -> semiExecEq ty1 sv1 sv2
        -- NotEq    -> semiExecNEq ty1 sv1 sv2
    
        -- sv1 is arr, sv2 is ix. Note that the translation inserts a
        -- check on the length, so probably we don't need an explicit
        -- check here.
        ArrayIndex -> semiExecArrayIndex g sv1 sv2 

        EmitArray -> unimplemented
          --  | Just (vsmbs,  bs) <- gseToList sv1          
          --  , Just (vsmarr, arrs) <- gseToList sv2 ->
          --      unless (isNothing (vsmLoopCountVar vsmbs)
          --              && isNothing (vsmLoopCountVar vsmarr)) $ panic "Invariant violated?" []
          --      pure $ singleton g (VSequence vsmbs (bs ++ arrs))
              
        EmitBuilder -> unimplemented

        -- sv1 is map, sv2 is key
        MapLookup -> mapOp g sv1 sv2
                           VNothing        (sNothing . symExecTy)
                           VJust           sJust 

        MapMember -> mapOp g sv1 sv2
                           (VValue $ V.VBool False)        (const (S.bool False))
                           (const (VValue $ V.VBool True)) (\_ _ -> S.bool True)

        ArrayStream -> unimplemented
                  
        -- Leq
        -- Lt
        -- Add
        -- Sub
        -- Mul
        -- Div
        -- Mod
        -- BitAnd
        -- BitOr
        -- BitXor
        -- Cat
        -- LCat
        -- LShift
        -- RShift

        -- Otherwise generate a symbolic term.
        _ -> def g sv1 sv2

    -- sv1 is map, sv2 is key 
    -- mapOp :: PathCondition -> GuardedSemiSExpr -> GuardedSemiSExpr ->
    --          GuardedSemiSExpr -> (Type -> SExpr) -> (GuardedSemiSExprs -> GuardedSemiSExprs) ->
    --          (SExpr -> SExpr -> SExpr) -> SemiSolverM m GuardedSemiSExprs

    -- FIXME: do something better here, i.e., return the values in the
    -- map guarded by the path expression to get there.
    mapOp g sv1 sv2 _missing _smissingf _found _sfound 
      -- -- Concrete case
      --  | Just els <- toL sv1, VValue kv <- sv2
      -- , Just res <- mapLookupV missing found kv els
      --   = pure res
      -- -- Expand into if-then-else.
      --  | Just els <- toL sv1
      -- , TMap kt vt <- ty1 = do
      --     tys <- asks typeDefs
      --     let symkv = toSExpr1 tys kt sv2
      --         mk    = sfound (symExecTy vt) . toSExpr1 tys vt
      --     vSExpr g rty (foldr (mapLookupS tys mk kt symkv sv2)
      --                         (smissingf vt)
      --                         els)
      | otherwise = def g sv1 sv2

    def g sv1 sv2 = do
      tys <- asks typeDefs
      vSExpr g rty =<< liftSolver (SE.symExecOp2 op ty1
                                   (toSExpr1 tys ty1 sv1)
                                   (toSExpr1 tys ty2 sv2))

    unimplemented = panic "semiEvalOp2: Unimplemented" [showPP op]

    -- mapLookupV z _f  _kv  [] = Just z
    -- mapLookupV z f  kv ((VValue kv', el) : rest) =
    --   if kv == kv' then Just (f el) else mapLookupV z f kv rest
    -- mapLookupV _ _ _ _ = Nothing

    -- mapLookupS tys f kTy symkv skv (skv', sel) rest =
    --   case (skv, skv') of
    --     (VValue kv, VValue kv')
    --       -> if kv == kv' then f sel else rest
    --     _ -> S.ite (S.eq symkv (toSExpr1 tys kTy skv')) (f sel) rest


semiExecArrayIndex :: SemiCtxt m => PathCondition ->
                      GuardedSemiSExpr ->
                      GuardedSemiSExpr ->
                      SemiSolverM m GuardedSemiSExprs
-- concrete spine, concrete index
semiExecArrayIndex g arrv (VValue v)
  | Just (vsm, svs) <- gseToList arrv
  , Just ix <- V.valueToIntSize v =
      let g' | Just lc <- vsmLoopCountVar vsm = PC.insertLoopCount lc (PC.LCCGt ix) g
             | otherwise = g
      in if length svs <= ix
         then hoistMaybe Nothing
         else hoistMaybe (refine g' (svs !! ix))

-- Symbolic index, we assert it at concrete indices and get n results.    
semiExecArrayIndex g arrv (VOther (Typed ty sIx))
  | Just (vsm, svs) <- gseToList arrv =
      let mkIxG ix =
            PC.insertValue sIx (Typed ty (VPCPositive $ PNum (fromIntegral ix))) g
            
          mkG' | Just lc <- vsmLoopCountVar vsm =
                   \ix -> PC.insertLoopCount lc (PC.LCCGt ix) (mkIxG ix)
               | otherwise = mkIxG

          mkV ix el = refine (mkG' ix) el
               
      in hoistMaybe (unions' (catMaybes (zipWith mkV [0..] svs)))

semiExecArrayIndex _g _arrv _sv = panic "BUG: saw non-symbolic sequence" []

semiExecOp3 :: SemiCtxt m => Op3 -> Type -> Type ->
               GuardedSemiSExprs -> GuardedSemiSExprs -> GuardedSemiSExprs ->
               SemiSolverM m GuardedSemiSExprs

-- We only need sv to be concrete here.
semiExecOp3 MapInsert rty@(TMap kt vt) _ty gvs k v =
  -- FIXME: this shouldn't fail, we gseCollect may be overkill
  gseCollect (map go (guardedValues gvs))
  where
    go (g, sv)
      | VMap els <- sv = pure $ singleton g (VMap ((k, v) : els))
      -- FIXME: this picks an ordering
      | VValue (V.VMap m) <- sv  = 
          let ms = [ (vValue mempty k', vValue mempty v')
                   | (k', v') <- Map.toList m ]
          in pure $ singleton g (VMap ((k, v) : ms))
      | VOther x <- sv = do
          tys <- asks typeDefs
          vSExpr g rty =<< liftSolver (SE.symExecOp3 MapInsert rty (S.const (typedThing x))
                                       (toSExpr tys kt k)
                                       (toSExpr tys vt v))
      | otherwise = panic "BUG: unexpected value shape" []

-- RangeUp and RangeDown      
semiExecOp3 op        _    _   _         _ _ = panic "Unimplemented" [showPP op]

semiExecOpN :: SemiCtxt m => OpN -> Type -> [GuardedSemiSExprs] ->
               SemiSolverM m GuardedSemiSExprs
semiExecOpN (ArrayL _ty) _rty vs =
  pure (singleton mempty (VSequence emptyVSequenceMeta vs))
semiExecOpN (CallF _fn) _rty _vs = panic "Impossible" []

-- -- -----------------------------------------------------------------------------
-- -- Value -> SExpr

-- FIXME: move
-- Probably an error if they don't match?
typeAtLabel :: Map TName TDecl -> Type -> Label -> Maybe (UserType, Type)
typeAtLabel tys (TUser ut) l
  | Just TDecl { tDef = TUnion flds }  <- tdecl = (,) ut <$> lookup l flds
  | Just TDecl { tDef = TStruct flds } <- tdecl = (,) ut <$> lookup l flds
  where
    tdecl = Map.lookup (utName ut) tys
typeAtLabel _ _ _ = Nothing

valueToSExpr :: Map TName TDecl -> Type -> V.Value -> SExpr
valueToSExpr tys ty v =
  case v of
    V.VUInt n i ->
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    V.VSInt n i -> -- FIXME: correct?
      if n `mod` 4 == 0
      then S.bvHex (fromIntegral n) i
      else S.bvBin (fromIntegral n) i
    V.VInteger i -> S.int i
    V.VBool b -> S.bool b
    V.VUnionElem l v' 
      | Just (ut, ty') <- typeAtLabel tys ty l
      -> S.fun (labelToField (utName ut) l) [go ty' v']

    -- FIXME: assumes the order is the same
    V.VStruct els
      | TUser ut <- ty
      , Just TDecl { tDef = TStruct flds } <- Map.lookup (utName ut) tys
      -> S.fun (typeNameToCtor (utName ut)) (zipWith goStruct els flds)
      | ty == TUnit -> sUnit
      
    V.VArray vs | TArray elty <- ty ->
      let sVals     = map (go elty) (Vector.toList vs)
          emptyArr = sArrayL (sEmptyL (symExecTy elty) (typeDefault elty)) -- FIXME, a bit gross?
          arr      = foldr (\(i, b) arr' -> S.store arr' (sSize i) b) emptyArr (zip [0..] sVals)
      in sArrayWithLength (symExecTy elty) arr (sSize (fromIntegral $ Vector.length vs))

    V.VMaybe mv | TMaybe ty' <- ty ->
      case mv of
        Nothing -> sNothing (symExecTy ty')
        Just v' -> sJust (symExecTy ty') (go ty' v')

    V.VMap m | TMap kt vt <- ty ->
      -- FIXME: breaks abstraction of maps
      sFromList (tTuple (symExecTy kt) (symExecTy vt))
                [ sTuple (go kt k) (go vt v') | (k, v') <- Map.toList m ]


    V.VStream {}                       -> unimplemented
    V.VBuilder vs
      | TBuilder elTy <- ty -> sFromList (symExecTy elTy) (reverse (map (go elTy) vs))

    -- FIXME: copied from semiSExprToSExpr
    V.VIterator vs -> case ty of
      -- FIXME: this breaks abstractions
      TIterator (TArray elTy) ->
        let emptyA = sEmptyL (symExecTy elTy) (typeDefault elTy)
            els    = [ (go sizeType k, go elTy v') | (k, v') <- vs ]
            arr    = foldr (\(i, v') arr' -> S.store arr' i v') emptyA els
        in case els of
          []             -> sArrayIterNew emptyA
          ((fstI, _) : _) -> S.fun "mk-ArrayIter" [arr, fstI]
      TIterator (TMap   _kt' _vt') -> panic "Unimplemented" []
      _ -> panic "Malformed iterator type" []

    _ -> unexpectedTy
  where
    unexpectedTy = panic "Unexpected type" [showPP v, showPP ty]

    go = valueToSExpr tys
    goStruct (l, e) (l', ty') | l == l' = go ty' e
    goStruct (l, _) (l', _) = panic "Mis-matched labels" [showPP l, showPP l']

    unimplemented = panic "Unimplemented" [showPP v]


-- ----------------------------------------------------------------------------------------
-- Helpers

orMany :: [SExpr] -> SExpr
orMany [] = S.bool False
orMany [x] = x
orMany xs  = S.orMany xs

andMany :: [SExpr] -> SExpr
andMany [] = S.bool True
andMany [x] = x
andMany xs  = S.andMany xs

-- -- -- Says whether a variable occurs in a SExpr, taking into account let binders.
-- -- freeInSExpr :: String -> SExpr -> Bool
-- -- freeInSExpr n = getAny . go
-- --   where
-- --     go (S.Atom a) = Any (a == n)
-- --     go (S.List [S.Atom "let", S.List es, e]) =
-- --       let (Any stop, occ) = goL es
-- --       in if stop then occ else occ <> go e

-- --     go (S.List es) = foldMap go es

-- --     goL (S.List [S.Atom a, be] : es) = (Any (a == n), go be) <> goL es
-- --     goL [] = (mempty, mempty)
-- --     goL _ = panic "Malformed let binding" []

-- -- freeInSemiSExpr :: String -> SemiSExpr -> Bool
-- -- freeInSemiSExpr n = getAny . foldMap (Any . freeInSExpr n)

-- -----------------------------------------------------------------------------
-- Instances


ppMuxValue :: (f a -> Doc) -> (a -> Doc) -> MuxValue f a -> Doc
ppMuxValue ppF ppA val =
  case val of
    VValue v -> pp v
    VOther v -> ppA v
    VUnionElem lbl v -> braces (pp lbl <.> colon <+> ppF v)
    VStruct xs      -> block "{" "," "}" (map ppFld xs)
      where ppFld (x,t) = pp x <.> colon <+> ppF t

    VSequence vsm vs ->  block (maybe "" (parens . pp) sid <> "[") "," "]" (map ppF vs)
      where sid = vsmGeneratorTag vsm
            
    VJust v   -> "Just" <+> ppF v
    VMap m -> block "{|" ", " "|}" [ ppF k <+> "->" <+> ppF v | (k,v) <- m ]

    VIterator vs -> block "[iterator|" ",        " "|]"
                    [ ppF x <+> "->" <+> ppF y | (x,y) <- vs ]

instance PP a => PP (GuardedValues a) where
  pp gvs = block "∨{" ";" "}" [ pp g <+> "⊨" <+> pp v | (g, v) <- guardedValues gvs ]

instance PP a => PP (GuardedValue a) where
  pp = ppMuxValue pp pp

