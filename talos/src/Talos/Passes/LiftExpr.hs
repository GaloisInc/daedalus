{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

-- ----------------------------------------------------------------------------------------
-- Lift parts of an expression to the grammar level, in particular
-- looping and recursive functions.  This also inlines any
-- (expression-level) function calls.
--
-- Ensures: no function calls in expressions, no loops in expressions

module Talos.Passes.LiftExpr (liftExprM) where

import           Control.Monad        (when, zipWithM, unless)
import           Control.Monad.Reader (ReaderT, asks, runReaderT)
import           Control.Monad.State  (StateT, modify, runStateT, state, gets)
import           Data.Bifunctor       (second)
import           Data.Foldable        (foldlM)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (isNothing)

import           Daedalus.Core
import           Daedalus.Core.Free   (FreeVars, freeFVars, freeVars)
import           Daedalus.Core.Type   (typeOf)
import           Daedalus.GUID
import           Daedalus.PP          (showPP)
import           Daedalus.Panic       (panic)
import           Daedalus.Rec         (forgetRecs, topoOrder)
import qualified Data.Set as Set
import Data.Functor (($>))

-- ----------------------------------------------------------------------------------------
-- Monad

data LiftExprEnv = LiftExprEnv
  { leeFFuns :: Map FName (Fun Expr)
  , leeFToG  :: Map FName FName
  , leeBFuns :: Map FName (Fun ByteSet)
  }

emptyEnv :: LiftExprEnv
emptyEnv = LiftExprEnv mempty mempty mempty

newtype LiftExprState = LiftExprState
  { lesNamed :: [(Name, Grammar)]
  }

type LiftExprM m = ReaderT LiftExprEnv (StateT LiftExprState m)
type LiftExprCtx m = (Monad m, HasGUID m)


runLiftExprM :: (Monad m) => LiftExprEnv -> LiftExprM m a -> m (a, [(Name, Grammar)])
runLiftExprM env m = second lesNamed <$> runStateT (runReaderT m env) emptySt
  where
    emptySt = LiftExprState []

-- | Add a new grammar node and get a name for it.
newNamed :: LiftExprCtx m => Grammar -> LiftExprM m Name
newNamed g = do
  n <- freshNameSys (typeOf g)
  newNamedWithName n g
  pure n

newNamedWithName :: LiftExprCtx m => Name -> Grammar -> LiftExprM m ()
newNamedWithName n g =
  modify (\s -> s { lesNamed = (n, g) : lesNamed s })

flushNamed :: LiftExprCtx m => LiftExprM m [(Name, Grammar)]
flushNamed = state (\s -> (lesNamed s, s { lesNamed = [] }))

freeInNamed :: LiftExprCtx m => Name -> LiftExprM m Bool
freeInNamed n = gets (any (Set.member n . freeVars . snd) . lesNamed)
  
-- -----------------------------------------------------------------------------
-- Workers

makeLiftedName :: LiftExprCtx m => FName -> m FName
makeLiftedName f = freshFName (f { fnameText = "Lifted_" <> fnameText f })

liftExprM :: LiftExprCtx m => Module -> m Module
liftExprM m = do
  when (hasRecursion (mBFuns m)) $ panic "Found ByteSet recrsion" []
  (efs, gfs, env) <- liftExprEFuns (mFFuns m)
  let env' = env { leeBFuns = Map.fromList [ (fName f, f) | f <- mBFuns m ] }
  gfs' <- mapM (liftExprGFun env') (mGFuns m)
  let allGFs = forgetRecs $ topoOrder (\f -> (fName f, freeFVars f)) (gfs ++ gfs')
  pure $ m { mFFuns = efs
           , mGFuns = allGFs
           , mBFuns = []
           }

-- For each recursive expr-function f we get Lifted_f, which is to be
-- defined later.  Note we don't _define_ the functions yet, just nmae
-- them.
liftExprEFuns :: LiftExprCtx m => [Fun Expr] ->
                 m ([Fun Expr], [Fun Grammar], LiftExprEnv)
liftExprEFuns oldFs = do
  (fs', gs', env') <- foldlM go ([], [], emptyEnv) ordered
  pure (reverse fs', reverse gs', env')
  where
    mkR (fs, gs, env) fn e_f' =
      case e_f' of
        Left efu  -> (efu : fs, gs, env { leeFFuns = Map.insert fn efu (leeFFuns env) })
        Right gfu -> (fs, gfu : gs, env { leeFToG  = Map.insert fn (fName gfu) (leeFToG env) })

    go r@(_, _, env) (NonRec fu) = mkR r (fName fu) <$> liftExprEFun env Nothing fu
    go r@(_, _, env) (MutRec fus) = do
      ns <- mapM (makeLiftedName . fName) fus
      let newFToGs = Map.fromList [ (fName fu, n) | (fu, n) <- zip fus ns ]
          env' = env { leeFToG = Map.union (leeFToG env) newFToGs }

      rs <- zipWithM (liftExprEFun env') (Just <$> ns) fus
      pure (foldl (uncurry . mkR) r (zip (map fName fus) rs))

    ordered = topoOrder (\f -> (fName f, freeFVars f)) oldFs
    -- The type etc. remains the same

-- We don't get the full env here, just the partial map of FFuns
liftExprEFun :: LiftExprCtx m => LiftExprEnv ->
                Maybe FName -> Fun Expr ->
                m (Either (Fun Expr) (Fun Grammar))
liftExprEFun env m_n fu =
  case fDef fu of
    Def e -> do
      (e', binds) <- runLiftExprM env (liftExprE e)
      if null binds && isNothing m_n
        then pure (Left fu { fDef = Def e' })
        else do -- Need to make a grammar
          fn' <- maybe (makeLiftedName (fName fu)) pure m_n
          let g' = bindNamed (Pure e') binds
              fu' = Fun { fName    = fn'
                        , fParams  = fParams fu
                        , fDef     = Def g'
                        , fIsEntry = False
                        , fAnnot   = fAnnot fu
                        }
          pure (Right fu')
    _ -> pure (Left fu)

liftExprGFun :: LiftExprCtx m => LiftExprEnv -> Fun Grammar ->
                m (Fun Grammar)
liftExprGFun env fu =
  case fDef fu of
    Def g -> do
      (g', binds) <- runLiftExprM env (liftExprG g)
      unless (null binds) $ panic "Binds should be flushed in liftExprG" []
      pure $ fu { fDef = Def g' }
    _ -> pure fu

liftExprE :: LiftExprCtx m => Expr -> LiftExprM m Expr
liftExprE expr = do
  e' <- childrenE liftExprE expr
  ffuns <- asks leeFFuns
  fToG  <- asks leeFToG
  case e' of
    -- Loops become grammars
    ELoop lm -> do
      let lm' = Pure <$> lm
      Var <$> newNamed (Loop (MorphismLoop lm'))

    PureLet n le re -> do
      nIsFree <- freeInNamed n
      if nIsFree
        then newNamedWithName n (Pure le) $> re
        else pure e'

    ApN (CallF f) es
      | Just gn <- Map.lookup f fToG -> Var <$> newNamed (Call gn es)
      | otherwise -> pure $ inlineCall f es ffuns

    _ -> pure e'

liftExprG :: LiftExprCtx m => Grammar -> LiftExprM m Grammar
-- This is treated specially as otherwise named grammars in e get
-- pushed into g
liftExprG (Let n e g) = do
  e' <- liftExprE e
  e_binds <- flushNamed
  g' <- liftExprG g
  pure $ bindNamed (Let n e' g') e_binds
liftExprG gram = do
  g <- gebChildrenG liftExprG liftExprE liftExprB gram
  -- We need to add the statements right at the grammar to capture at
  -- the right place.
  bindNamed g <$> flushNamed

liftExprB :: LiftExprCtx m => ByteSet -> LiftExprM m ByteSet
liftExprB bs = do
  bs' <- ebChildrenB liftExprE liftExprB bs
  bfuns <- asks leeBFuns
  case bs' of
    -- we don't pre-inline bytesets as we might need to add grammar
    -- stuff, so we need to recurse into the new thing.  We need to
    -- check before hand there is no recursion however (which doesn't
    -- really make sense for bytesets?)
    SetCall f es -> liftExprB (inlineCall f es bfuns)
    _            -> pure bs'

-- -----------------------------------------------------------------------------
-- Helpers

inlineCall :: CoreSyn a => FName -> [Expr] -> Map FName (Fun a) -> a
inlineCall f es env
  | Just Fun { fDef = Def e, fParams = ps } <- Map.lookup f env =
      foldl (\acc (n, e') -> coreLet n e' acc) e (zip ps es)
  | otherwise = panic "Missing function definition" [showPP f]

hasRecursion :: FreeVars a => [Fun a] -> Bool
hasRecursion fs = any isMutRec ordered
  where
    isMutRec MutRec {} = True
    isMutRec _ = False

    ordered = topoOrder (\f -> (fName f, freeFVars f)) fs

bindNamed :: Grammar -> [(Name, Grammar)] -> Grammar
bindNamed = foldr (\(n, lhs) acc -> Do n lhs acc)
