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

-- -----------------------------------------------------------------------------
-- Semi symbolic/concrete evaluation

module Talos.Strategy.MuxValue where

import           Control.Lens                 (locally, over)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe    (MaybeT, runMaybeT)
import           Data.Foldable                (foldlM)
import           Data.Functor                 (($>))
import           Data.Generics.Product        (field)
import           Data.List                    (transpose)
import           Data.List.NonEmpty           (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty           as NE
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Map.Merge.Lazy          as Map
import           Data.Maybe                   (catMaybes, mapMaybe)
import           Data.Semigroup               (sconcat)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import qualified Data.Vector                  as Vector
import           GHC.Generics                 (Generic)
import           GHC.Stack                    (HasCallStack)

import           SimpleSMT                    (SExpr)
import qualified SimpleSMT                    as S

import           Daedalus.Core                hiding (freshName)
import qualified Daedalus.Core.Semantics.Env  as I
import           Daedalus.Core.Semantics.Expr (evalOp1, evalOp2, matches, partial, evalOp0)
import           Daedalus.Core.Type
import           Daedalus.GUID
import           Daedalus.PP
import           Daedalus.Panic
import qualified Daedalus.Value.Type          as V

-- import Talos.Strategy.Monad
import qualified Talos.SymExec.Expr           as SE
import           Talos.SymExec.SolverT
import           Talos.SymExec.StdLib
import           Talos.SymExec.Type

--------------------------------------------------------------------------------
-- Value Guards

-- -- These are the things which cause branching
-- data SymPathElement =
--   SPEChoose PathVar Integer
--   | SPECase SMTVar  Type Pattern

-- pathToSExpr :: SymPath -> SExpr
-- pathToSExpr els = S.andMany (map go els)
--   where
--     go (SPEChoose v n)    = S.eq v (S.int n)
--     go (SPECase v ty pat) = SE.patternToPredicate ty pat v

-- -- in reverse order (most recent choice is the head)
-- type SymPath = [SymPathElement]

newtype PathVar = PathVar SMTVar
  deriving (Eq, Ord, Show)

pathVarToSExpr :: PathVar -> SExpr
pathVarToSExpr (PathVar v) = S.const v

data ValueGuardCaseInfo = ValueGuardCaseInfo
  { vgciType       :: Type
  , vgciConstraint :: Either Pattern (Set Pattern)
  } deriving (Generic)

data ValueGuard = ValueGuard
  { vgCases   :: Map SMTVar ValueGuardCaseInfo
  -- ^ Case match path conditions.
  , vgChoices :: Map PathVar Int
  -- ^ We call these out separately as it is easy to figure out if the
  -- conjunction of two guards is unsat.
  } deriving (Generic)

conjCases :: ValueGuardCaseInfo -> ValueGuardCaseInfo ->
             Maybe ValueGuardCaseInfo
conjCases ci1 ci2 =
  case (vgciConstraint ci1, vgciConstraint ci2) of
    (Left p1, Left p2)  -> guard (p1 == p2) $> ci1
    (Left p, Right nps) -> guard (p `Set.notMember` nps) $> ci1
    (Right nps, Left p) -> guard (p `Set.notMember` nps) $> ci2
    (Right nps1, Right nps2) -> Just (ci1 { vgciConstraint = Right (Set.union nps1 nps2) })

-- | Conjoins two guards, returning Nothing if the result is unsat.
-- This is an optimisation, so an unsat guards may still be returned.
conjValueGuard :: ValueGuard -> ValueGuard -> Maybe ValueGuard
conjValueGuard vg1 vg2 =
  ValueGuard <$> joinCases   (vgCases vg1) (vgCases vg2)
             <*> joinChoices (vgChoices vg1) (vgChoices vg2)
  where
    joinCases =
      Map.mergeA Map.preserveMissing Map.preserveMissing
          (Map.zipWithAMatched (const conjCases))
          
    -- Copied from mergeUnifiers in MemoSearch.hs, should probably be a library thing.
    joinChoices =
      Map.mergeA Map.preserveMissing Map.preserveMissing
          (Map.zipWithAMatched (\_k x y -> guard (x == y) $> x))

valueGuardInsertCase :: SMTVar -> ValueGuardCaseInfo ->
                        ValueGuard -> Maybe ValueGuard
valueGuardInsertCase v els = conjValueGuard (ValueGuard (Map.singleton v els) mempty)

valueGuardInsertChoice :: PathVar -> Int ->
                          ValueGuard -> Maybe ValueGuard
valueGuardInsertChoice v el =
  conjValueGuard (ValueGuard mempty (Map.singleton v el))

valueGuardExtendChoice :: PathVar -> Int ->
                          ValueGuard -> ValueGuard
valueGuardExtendChoice v el =
  over (field @"vgChoices") (Map.insertWith (\_ _ -> panic "Duplicate key" []) v el)
  
-- | The trivially satisfiable guard (i.e., unconstrained)
trueValueGuard :: ValueGuard
trueValueGuard = ValueGuard mempty mempty

valueGuardToSExpr :: ValueGuard -> SExpr
valueGuardToSExpr vg = S.andMany (cases ++ choices)
  where
    notf b = if b then id else S.not
    cases = [ notf b (SE.patternToPredicate (vgciType vgci) p (S.const n))
            | (n, vgci) <- Map.toList (vgCases vg)
            , (b, p) <- either (\p -> [(True, p)])
                               (map ((,) False) . Set.toList)
                               (vgciConstraint vgci)
            ]
    choices = [ S.eq (pathVarToSExpr pv) (S.int (fromIntegral i))
              | (pv, i) <- Map.toList (vgChoices vg)
              ]
              
--------------------------------------------------------------------------------
-- GuardedValues

-- | A GuardedValues represents a value along a number of paths.

-- sjw: I think no two guards should be simultaneously satisfiable, as
-- tha would mean we would be on 2 paths at the same time.  This might
-- not be the case if we have duplicates, or the same valye with
-- different guards.
newtype GuardedValues a = GuardedValues {
  getGuardedValues :: NonEmpty (ValueGuard, GuardedValue a)
  }

guardedValues :: GuardedValues a -> [(ValueGuard, GuardedValue a)]
guardedValues = NE.toList . getGuardedValues
  
type GuardedValue a = MuxValue GuardedValues a

data MuxValue f a =
    VValue                 !V.Value
  | VOther                 !a
  | VUnionElem             !V.Label !(f a)
  -- We don't need to support partial updates (e.g. x { foo = bar }
  -- where x is symbolic) as Daedalus doesn't (yet) support updates.
  | VStruct                ![(V.Label, f a)]

  -- The bool is used to tell us if this is an array or builder, use in toValue
  | VSequence              Bool ![f a]
  | VJust                  !(f a)

  -- -- For iterators and maps
  -- | VPair                  !(f a) !(f a)
  
  -- We support symbolic keys, so we can't use Map here

  | VMap                   ![(f a, f a)]
  | VIterator              ![(f a, f a)]
    deriving (Show, Eq, Ord, Foldable, Traversable, Functor)

-- Typed so we can turn into a value.
type GuardedSemiSExprs = GuardedValues (Typed SMTVar)
type GuardedSemiSExpr  = GuardedValue  (Typed SMTVar)

singletonGuardedValues :: ValueGuard -> GuardedSemiSExpr -> GuardedSemiSExprs
singletonGuardedValues g gv = GuardedValues ((g, gv) :| [])

muxValueToList :: (V.Value -> f a) -> MuxValue f a -> Maybe [f a]
muxValueToList mk mv =
  case mv of
    VSequence _ xs         -> Just xs
    VValue (V.VArray v)    -> Just (map mk (Vector.toList v))
    -- Builders are stored in reverse order.
    VValue (V.VBuilder vs) -> Just (map mk (reverse vs))
    _ -> Nothing

gseToList :: GuardedSemiSExpr -> Maybe [GuardedSemiSExprs]
gseToList = muxValueToList (vValue trueValueGuard)

-- -----------------------------------------------------------------------------
-- Values

vUnit :: GuardedSemiSExprs
vUnit = singletonGuardedValues trueValueGuard $ VValue V.vUnit

vOther :: ValueGuard -> Typed SMTVar -> GuardedSemiSExprs
vOther g = singletonGuardedValues g . VOther 

vSExpr :: MonadSolver m => ValueGuard -> Type -> SExpr -> m GuardedSemiSExprs
vSExpr g ty se = vOther g . Typed ty <$> sexprAsSMTVar (symExecTy ty) se

vValue :: ValueGuard -> V.Value -> GuardedSemiSExprs
vValue g = singletonGuardedValues g . VValue

vBool :: ValueGuard -> Bool -> GuardedSemiSExprs
vBool g = vValue g . V.VBool

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


refineGuardedSemiExprs :: ValueGuard -> GuardedSemiSExprs -> Maybe GuardedSemiSExprs
refineGuardedSemiExprs g gvs =
  GuardedValues <$> nonEmpty (catMaybes [ (, v) <$> conjValueGuard g g'
                                        | (g', v) <- guardedValues gvs ])

unionGuardedSemiExprs :: NonEmpty GuardedSemiSExprs -> GuardedSemiSExprs
unionGuardedSemiExprs = GuardedValues . sconcat . fmap getGuardedValues

unionGuardedSemiExprs' :: [GuardedSemiSExprs] -> Maybe GuardedSemiSExprs
unionGuardedSemiExprs' = fmap unionGuardedSemiExprs . nonEmpty

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
--   vSExpr trueValueGuard TBool <$> symExecToSemiExec (SE.symExecByteSet b byteSet) -- go byteSet
  -- where
  --   go bs = case bs of
  --     SetAny         -> pure (vBool trueValueGuard True)
  --     SetSingle e    -> semiExecEq byteT b =<<
  --                         (vSExpr trueValueGuard TBool =<< semiExecExpr e)
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
symExecToSemiExec = lift . withReaderT envf
  where
    envf env = envToSymEnv (typeDefs env) (localBoundNames env)

-- symExec :: (HasGUID m, Monad m, MonadIO m) => Expr -> SemiSolverM m SemiSExpr
-- symExec e = vSExpr (typeOf e) <$> symExecToSemiExec (SE.symExecExpr e)

symExecByteSet :: (HasGUID m, Monad m, MonadIO m) => ByteSet -> SExpr -> SemiSolverM m GuardedSemiSExprs
symExecByteSet bs b = do
  se <- symExecToSemiExec (SE.symExecByteSet b bs)
  vSExpr trueValueGuard TBool se

envToSymEnv :: Map TName TDecl -> Map Name GuardedSemiSExprs ->  Map Name SExpr
envToSymEnv tenv = Map.mapWithKey (guardedSemiSExprsToSExpr tenv . nameType)

-- ASSUME: one guard is true, so the negation of all but one entails
-- the remaining guard.
guardedSemiSExprsToSExpr :: Map TName TDecl -> Type ->
                            GuardedSemiSExprs -> SExpr
guardedSemiSExprsToSExpr tys ty gvs =
  foldr (\(g', el') -> S.ite (valueGuardToSExpr g') (go el')) (go el) els
  where
    go = guardedSemiSExprToSExpr tys ty
    (_g, el) :| els = getGuardedValues gvs

guardedSemiSExprToSExpr :: Map TName TDecl -> Type ->
                           GuardedSemiSExpr -> SExpr
guardedSemiSExprToSExpr tys ty sv = 
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

    VSequence _ vs
      | Just elTy <- typeToElType ty ->
          sArrayLit (symExecTy elTy) (typeDefault elTy) (map (go elTy) vs)
          
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
    go = guardedSemiSExprsToSExpr tys
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
matches' :: Type -> GuardedSemiSExpr -> Pattern -> Bool
matches' ty v pat =
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
semiExecCase :: (Monad m, HasGUID m, HasCallStack) =>
                Case a ->
                SemiSolverM m ( [ ( NonEmpty ValueGuard, a) ], [ (ValueGuard, SExpr) ] )
semiExecCase (Case y pats) = do
  els <- guardedValues <$> semiExecName y
  let (vgs, preds) = unzip (map goV els)
      vgs_for_pats = map (nonEmpty . catMaybes) (transpose vgs)
      allRes       = zipWith (\a -> fmap (, a)) (map snd pats) vgs_for_pats
  pure (catMaybes allRes, concat preds)
  where
    goV :: (ValueGuard, GuardedSemiSExpr) ->
           ( [ Maybe ValueGuard ], [(ValueGuard, SExpr)] )
    goV (g, VOther x) =
      let basePreds = map (\p -> SE.patternToPredicate ty p (S.const (typedThing x))) pats'
          (m_any, assn)
            -- if we have a default case, the constraint is that none
            -- of the above matched
            | hasAny    = ([ Right (Set.fromList pats') ] , [])
            | otherwise = ([], [(g, S.orMany basePreds)])
          vgciFor c = ValueGuardCaseInfo { vgciType = ty, vgciConstraint = c }
      in (map (\c -> valueGuardInsertCase (typedThing x) (vgciFor c) g) (map Left pats' ++ m_any), assn)
    goV (g, v) =
      let ms = map (matches' ty v) pats'
          noMatch = not (or ms)
          (m_any, assn)
            | hasAny && noMatch = ( [True], [] )
            | hasAny            = ( [False], [] )
            | noMatch           = ( [], [(g, S.bool False)] )
            -- FIXME: do we need the (g, S.bool True) here?
            | otherwise         = ( [], [(g, S.bool True)] )
      in ( [ guard b $> g | b <- ms ++ m_any], assn )

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
  , funDefs         :: Map FName (Fun Expr)
  , byteFunDefs     :: Map FName (Fun ByteSet)
  } deriving (Generic)


typeDefs :: SemiSolverEnv -> Map TName TDecl
typeDefs = I.tEnv . interpEnv

type SemiCtxt m = (Monad m, MonadIO m, HasGUID m)

type SemiSolverM m = MaybeT (ReaderT SemiSolverEnv (SolverT m))

runSemiSolverM :: SemiCtxt m =>
                  Map FName (Fun Expr) ->
                  Map FName (Fun ByteSet) ->                  
                  Map Name GuardedSemiSExprs ->
                  I.Env ->
                  SemiSolverM m a -> SolverT m (Maybe a)
runSemiSolverM funs bfuns lenv env m =
  runReaderT (runMaybeT m) (SemiSolverEnv lenv env funs bfuns)

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
gseCollect gvs = collectMaybes gvs >>= hoistMaybe . unionGuardedSemiExprs'

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
      pure (singletonGuardedValues trueValueGuard $ VStruct (zip ls sves))

    ECase cs -> do
      (ms, _partialPred) <- semiExecCase cs
      let mk1 (gs, e) = do
            v <- semiExecExpr e
            pure $ mapMaybe (\g -> refineGuardedSemiExprs g v) (NE.toList gs)
      els <- concat <$> mapM mk1 ms
      hoistMaybe (unionGuardedSemiExprs' els)
        
    Ap0 op       -> pure (vValue trueValueGuard $ partial (evalOp0 op))
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
  pure . singletonGuardedValues trueValueGuard $ VJust gvs

semiExecOp1 (InUnion _ut l)  _rty _ty sv =
  pure . singletonGuardedValues trueValueGuard $ VUnionElem l sv

-- We do this component-wise
semiExecOp1 op rty ty gvs = gseCollect (map go (guardedValues gvs))
  where
    mk1 g = pure . singletonGuardedValues g
    toL = gseToList
    
    -- This function executes a for a single value, returning Nothing
    -- if the value is of the wrong shape (e.g. not Just).  It is the
    -- responsibility of the context to make sure that the right
    -- assertions are in place so that ignoring results is OK.  In
    -- practice, this means that partial operations are guarded by a
    -- case.
    go (g, VOther x) =
      vSExpr g ty =<< liftSolver (SE.symExecOp1 op ty (S.const (typedThing x)))
  
    -- Value has a concrete head
    go (g, sv) = case op of
      IsEmptyStream -> unimplemented
      Head          -> unimplemented
      StreamOffset  -> unimplemented
      StreamLen     -> unimplemented
      ArrayLen
        | Just svs <- toL sv -> mk1 g $ VValue (V.vSize (toInteger (length svs)))
      Concat | Just svs  <- toL sv -> hoistMaybe (gseConcat g svs)
      -- Just flip the 'isbuilder' flag
      FinishBuilder | VSequence {} <- sv -> mk1 g sv
      NewIterator
        | TArray {} <- ty
        , Just svs  <- toL sv ->
          let ixs = map (vValue trueValueGuard . V.vSize) [0..]
              els = zip ixs svs
          in mk1 g $ VIterator els
          
      NewIterator -> unimplemented -- maps
      IteratorDone | VIterator els <- sv -> pure (vBool g (null els))
      IteratorKey
        | VIterator ((k, _) : _) <- sv  ->
            hoistMaybe (refineGuardedSemiExprs g k) -- FIXME: correct?
        | VIterator [] <- sv -> hoistMaybe Nothing
        
      IteratorVal
        | VIterator ((_, v) : _) <- sv ->
            hoistMaybe (refineGuardedSemiExprs g v) -- FIXME: correct?
        | VIterator [] <- sv -> hoistMaybe Nothing 
            
      IteratorNext
        | VIterator (_ : els) <- sv -> mk1 g (VIterator els)
        | VIterator [] <- sv -> hoistMaybe Nothing

      -- handled above.
      -- EJust -> mk1 g (VJust sv)
      FromJust
        | VJust sv' <- sv -> hoistMaybe (refineGuardedSemiExprs g sv')
        | VNothing <- sv  -> hoistMaybe Nothing
  
      SelStruct _ty l
        | VStruct flds <- sv
        , Just sv' <- lookup l flds -> hoistMaybe (refineGuardedSemiExprs g sv')
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
gseConcat :: ValueGuard -> [GuardedSemiSExprs] -> Maybe GuardedSemiSExprs
gseConcat g svs = unionGuardedSemiExprs' (mapMaybe mkOne combs)
  where
    elss = map guardedValues svs
    -- This is a sneaky way to get the list of products of the
    -- members.  This is potentially very expensive.
    combs = sequence elss

    doConcat els g'
      | Just els' <- mapM toL els = singletonGuardedValues g' (VSequence False (concat els'))
      | otherwise  = panic "UNIMPLEMENTED: saw symbolic list" []
    
    mkOne comb =
      let (gs, els) = unzip comb
      in doConcat els <$> foldlM conjValueGuard g gs

    toL = gseToList

sexprAsSMTVar :: MonadSolver m => SExpr -> SExpr -> m SMTVar
sexprAsSMTVar _ty (S.Atom x) = pure x
sexprAsSMTVar ty e = liftSolver (defineSymbol "named" ty e)

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

bOpMany :: SemiCtxt m => Bool -> ValueGuard -> 
           [GuardedSemiSExprs] ->
           SemiSolverM m GuardedSemiSExprs
bOpMany opUnit g svs = gseCollect (map mkOne combs)
  where
    elss = map guardedValues svs
    -- This is a sneaky way to get the list of products of the
    -- members.  This is potentially very expensive.
    combs = sequence elss
    
    mkOne comb =
      let (gs, els) = unzip comb
      in go els =<< hoistMaybe (foldlM conjValueGuard g gs)

    go els g' 
      | any (isBool absorb) els = mkB g' absorb
      | otherwise = do
          tys <- asks typeDefs
          -- strip out units and convert to sexpr
          let nonUnits = [ guardedSemiSExprToSExpr tys TBool sv
                         | sv <- els, not (isBool opUnit sv) ]
          if null nonUnits
            then mkB g' opUnit
            else vSExpr g TBool (op nonUnits)

    mkB g' = pure . vBool g'

    isBool b (VValue (V.VBool b')) = b == b'
    isBool _ _ = False
    op = if opUnit then S.andMany else S.orMany
    absorb = not opUnit

bAndMany, bOrMany :: SemiCtxt m => ValueGuard -> 
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
             , Just g    <- [ conjValueGuard g1 g2 ]
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
      
        _ | Just svs1 <- toL sv1
          , Just svs2 <- toL sv2
          , Just elTy <- typeToElType ty ->
            if length svs1 /= length svs2
            then mkB (not iseq)
            else opMany g =<< zipWithM (semiExecEqNeq iseq elTy) svs1 svs2
            
        _ -> do
          se <- liftSolver (SE.symExecOp2 op TBool
                            (guardedSemiSExprToSExpr tys ty sv1)
                            (guardedSemiSExprToSExpr tys ty sv2))
          vSExpr g TBool se
            
    toL = gseToList

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
    mk g vs = (g, VSequence True (vs ++ [gvs2]))
    (gs, gvss1) = NE.unzip (getGuardedValues gvs1)
    
semiExecOp2 op rty ty1 ty2 gvs1 gvs2 =
  gseCollect [ go g sv1 sv2
             | (g1, sv1) <- guardedValues gvs1
             , (g2, sv2) <- guardedValues gvs2
             , Just g    <- [ conjValueGuard g1 g2 ]
             ]
  where
    -- point-wise
    go g (VValue v1) (VValue v2) =
      pure (singletonGuardedValues g (VValue (evalOp2 op v1 v2)))

    -- Some of these are lazy so we don't eagerly convert to sexpr if
    -- only one side is a sexpr
    go g sv1 sv2 = do
      case op of
        IsPrefix -> unimplemented
        Drop     -> unimplemented
        Take     -> unimplemented

        -- Eq       -> semiExecEq ty1 sv1 sv2
        -- NotEq    -> semiExecNEq ty1 sv1 sv2
    
        -- sv1 is arr, sv2 is ix
        ArrayIndex
          -- concrete spine, concrete index
          | Just svs <- toL sv1
          , VValue v <- sv2, Just ix <- V.valueToIntSize v
            -> hoistMaybe (refineGuardedSemiExprs g (svs !! ix))

          -- FIXME: produce a n-way value (I doubt this case is very common)

          -- concrete spine, symbolic index.  We could either make the
          -- whole thing symbolic, or create a value for each value in
          -- the list and also constrain the index.
          --  | Just svs <- toL sv1
          --  , VOther x <- sv2
          --    -> hoistMaybe (refineGuardedSemiExprs g (svs !! ix))
            
        EmitArray   -> unimplemented
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
    -- mapOp :: ValueGuard -> GuardedSemiSExpr -> GuardedSemiSExpr ->
    --          GuardedSemiSExpr -> (Type -> SExpr) -> (GuardedSemiSExprs -> GuardedSemiSExprs) ->
    --          (SExpr -> SExpr -> SExpr) -> SemiSolverM m GuardedSemiSExprs

    -- FIXME: do something better here, i.e., return the values in the
    -- map guarded by the path expression to get there.
    mapOp g sv1 sv2 missing smissingf found sfound 
      -- -- Concrete case
      -- | Just els <- toL sv1, VValue kv <- sv2
      -- , Just res <- mapLookupV missing found kv els
      --   = pure res
      -- -- Expand into if-then-else.
      -- | Just els <- toL sv1
      -- , TMap kt vt <- ty1 = do
      --     tys <- asks typeDefs
      --     let symkv = guardedSemiSExprToSExpr tys kt sv2
      --         mk    = sfound (symExecTy vt) . guardedSemiSExprToSExpr tys vt
      --     vSExpr g rty (foldr (mapLookupS tys mk kt symkv sv2)
      --                         (smissingf vt)
      --                         els)
      | otherwise = def g sv1 sv2

    def g sv1 sv2 = do
      tys <- asks typeDefs
      vSExpr g rty =<< liftSolver (SE.symExecOp2 op ty1
                                   (guardedSemiSExprToSExpr tys ty1 sv1)
                                   (guardedSemiSExprToSExpr tys ty2 sv2))

    toL = gseToList

    unimplemented = panic "semiEvalOp2: Unimplemented" [showPP op]

    mapLookupV z _f  _kv  [] = Just z
    mapLookupV z f  kv ((VValue kv', el) : rest) =
      if kv == kv' then Just (f el) else mapLookupV z f kv rest
    mapLookupV _ _ _ _ = Nothing

    mapLookupS tys f kTy symkv skv (skv', sel) rest =
      case (skv, skv') of
        (VValue kv, VValue kv')
          -> if kv == kv' then f sel else rest
        _ -> S.ite (S.eq symkv (guardedSemiSExprToSExpr tys kTy skv')) (f sel) rest


semiExecOp3 :: SemiCtxt m => Op3 -> Type -> Type ->
               GuardedSemiSExprs -> GuardedSemiSExprs -> GuardedSemiSExprs ->
               SemiSolverM m GuardedSemiSExprs

-- We only need sv to be concrete here.
semiExecOp3 MapInsert rty@(TMap kt vt) _ty gvs k v =
  -- FIXME: this shouldn't fail, we gseCollect may be overkill
  gseCollect (map go (guardedValues gvs))
  where
    go (g, sv)
      | VMap els <- sv = pure $ singletonGuardedValues g (VMap ((k, v) : els))
      -- FIXME: this picks an ordering
      | VValue (V.VMap m) <- sv  = 
          let ms = [ (vValue trueValueGuard k', vValue trueValueGuard v')
                   | (k', v') <- Map.toList m ]
          in pure $ singletonGuardedValues g (VMap ((k, v) : ms))
      | VOther x <- sv = do
          tys <- asks typeDefs
          vSExpr g rty =<< liftSolver (SE.symExecOp3 MapInsert rty (S.const (typedThing x))
                                       (guardedSemiSExprsToSExpr tys kt k)
                                       (guardedSemiSExprsToSExpr tys vt v))
      | otherwise = panic "BUG: unexpected value shape" []

-- RangeUp and RangeDown      
semiExecOp3 op        _    _   _         _ _ = panic "Unimplemented" [showPP op]

semiExecOpN :: SemiCtxt m => OpN -> Type -> [GuardedSemiSExprs] ->
               SemiSolverM m GuardedSemiSExprs
semiExecOpN (ArrayL _ty) _rty vs =
  pure (singletonGuardedValues trueValueGuard (VSequence False vs))
               
-- semiExecOpN op rty vs
--   | Just vs' <- mapM unValue vs = do
--       env <- asks interpEnv
--       pure (VValue (evalOpN op vs' env))
--   | Just vs' <- mapM unOther vs = lift (vSExpr rty <$> SE.symExecOpN op vs')
--   where
--     unValue (VValue v) = Just v
--     unValue _ = Nothing

--     unOther (VOther v) = Just (typedThing v)
--     unOther _ = Nothing

-- Unfold body of function.

-- FIXME: This may run into termination issues if the arguments aren't
-- concrete enough (e.g. map over a symbolic array)
semiExecOpN (CallF fn) _rty vs = do
  fdefs <- asks funDefs
  let (ps, e) = case Map.lookup fn fdefs of
        Just fdef | Def d <- fDef fdef -> (fParams fdef, d)
        _   -> panic "Missing function " [showPP fn]

  foldr (uncurry bindNameIn) (semiExecExpr e) (zip ps vs)

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
