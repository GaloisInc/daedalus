{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

-- ----------------------------------------------------------------------------------------
-- Lift parts of an expression to the grammar level, in particular
-- looping and recursive functions.  This also inlines any
-- (expression-level) function calls.
--
-- Ensures: no function calls in expressions, no loops in expressions

module Talos.Passes.NoBytesPatterns (noBytesPatternsM) where

import           Data.Foldable      (foldrM)
import           Data.List          (partition)

import           Daedalus.Core
import           Daedalus.Core.Type (typeOf)
import           Daedalus.GUID
import           Daedalus.Panic     (panic)
import           Daedalus.PP        (showPP)

type NoBytesPatternCtx m = (Monad m, HasGUID m)
  
-- -----------------------------------------------------------------------------
-- Workers

noBytesPatternsM :: NoBytesPatternCtx m => Module -> m Module
noBytesPatternsM m = do
  gfs' <- mapM (nbpFun nbpG) (mGFuns m)
  ffs' <- mapM (nbpFun nbpE) (mFFuns m)
  bfs' <- mapM (nbpFun nbpB) (mBFuns m)  
  pure $ m { mFFuns = ffs'
           , mGFuns = gfs'
           , mBFuns = bfs'
           }

-- For each recursive expr-function f we get Lifted_f, which is to be
-- defined later.  Note we don't _define_ the functions yet, just nmae
-- them.
nbpFun :: NoBytesPatternCtx m => (a -> m a) -> Fun a -> m (Fun a)
nbpFun f fu = do
  def' <- case fDef fu of
            Def v -> Def <$> f v
            def    -> pure def
  pure (fu { fDef = def' })

nbpCase :: (CoreSyn a, NoBytesPatternCtx m) => a -> Case a -> m a
nbpCase dflt (Case n pats)
  | any (isBytesPattern . fst) pats = foldrM mkOne dflt' otherPats
  | otherwise = pure (coreCase n pats)
  where
    mkOne (PBytes bs, tc) fc = do
      v <- freshNameSys TBool
      pure (coreLet v (eq (Var n) (byteArrayL bs))
             (coreIf n tc fc))

    mkOne (pat, _) _ = panic "Unexpected pattern: expected PBytes" [showPP pat]
    isBytesPattern PBytes {} = True
    isBytesPattern _         = False
    
    (dfltPats, otherPats) = partition ((==) PAny . fst) pats
    dflt' | (_, a) : _ <- dfltPats = a
          | otherwise              = dflt

nbpE :: NoBytesPatternCtx m => Expr -> m Expr
nbpE expr = do
  expr' <- childrenE nbpE expr
  case expr' of
    ECase cs -> nbpCase (panic "Missing default case in PBytes case" []) cs
    _ -> pure expr'

-- This is in here mainly for convenience.  Invariant: always writes mempty
nbpG :: NoBytesPatternCtx m => Grammar -> m Grammar
nbpG gram = do
  gram' <- gebChildrenG nbpG nbpE nbpB gram
  case gram' of
    GCase cs -> nbpCase (Fail ErrorFromSystem (typeOf gram) Nothing) cs
    _ -> pure gram'

nbpB :: NoBytesPatternCtx m => ByteSet -> m ByteSet
nbpB bs = do
  bs' <- ebChildrenB nbpE nbpB bs
  case bs' of
    SetCase cs   -> nbpCase (panic "Missing default case in PBytes case" []) cs
    _            -> pure bs'
