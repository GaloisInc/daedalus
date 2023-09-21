
-- This is a hack so that the analysis works when we have constant arguments.

module Talos.Passes (nameBoundExprM, removeUnitsM, nameMatchResultsM, allPassesM ) where

import Daedalus.GUID
import Daedalus.Core
import Daedalus.Core.Type
import Daedalus.Core.Normalize
import Daedalus.Core.CFG (addNodeIDs)

-- import Daedalus.Core.Inline
import qualified Data.Text as Text
import Daedalus.PP (showPP)
import Control.Monad (zipWithM)

import Talos.Passes.LiftExpr (liftExprM)
import Talos.Passes.NoBytesPatterns (noBytesPatternsM)


allPassesM :: (Monad m, HasGUID m) => FName -> Module -> m Module
allPassesM _entry m = noBytesPatternsM m >>=
                      pure . removeUnitsM >>=
                      liftExprM >>=
                      nameBoundExprM >>=
                      nameMatchResultsM >>=
                      pure . normM >>=
                      pure . letToDoM >>=
                      addNodeIDs


-- ----------------------------------------------------------------------------------------
-- Just turn Let into Do (this has to happen after normM)

letToDoM :: Module -> Module
letToDoM mo = mo { mGFuns = gfs }
  where
    gfs = map (fmap letToDoG) (mGFuns mo)

letToDoG :: Grammar -> Grammar
letToDoG (Let n e g) = Do n (Pure e) (letToDoG g)
letToDoG g = gebMapChildrenG letToDoG id id g

-- ----------------------------------------------------------------------------------------
-- Name non-variable bound expressions
--
-- The analysis is simpler if we name all non-Do bound expressions.
-- This allows the analysis to assume that slices start at Do/Let
-- binders only.

nameBoundExprM :: (Monad m, HasGUID m) => Module -> m Module
nameBoundExprM m = do
  gfuns' <- mapM nameBoundExprGFun (mGFuns m)
  pure (m { mGFuns = gfuns' })

nameBoundExprGFun :: (Monad m, HasGUID m) => Fun Grammar -> m (Fun Grammar)
nameBoundExprGFun fu =
  case fDef fu of
    Def g -> (\g' -> fu { fDef = Def g' }) <$> nameBoundExprG g
    _     -> pure fu

nameBoundExprG :: (Monad m, HasGUID m) => Grammar -> m Grammar
nameBoundExprG gram = do
  gram' <- childrenG nameBoundExprG gram
  case gram' of
    Loop lclass ->
      case lclass of
        ManyLoop {} -> pure gram'
        RepeatLoop bt n e g -> do
          (bs, e') <- nameBound ("_r" ++ showPP n) e
          pure (doBind (Loop (RepeatLoop bt n e' g)) bs)
          
        MorphismLoop (FoldMorphism n e lc g) -> do
          (bse, e') <- nameBound ("_acc" ++ showPP n) e
          (bslc, lc') <- (\(bs, col) -> (bs, lc { lcCol = col })) <$> nameBound "_col" (lcCol lc)
          pure (doBind (Loop (MorphismLoop (FoldMorphism n e' lc' g))) (bse ++ bslc))

        MorphismLoop (MapMorphism lc g) -> do
          (bslc, lc') <- (\(bs, col) -> (bs, lc { lcCol = col })) <$> nameBound "_col" (lcCol lc)
          pure (doBind (Loop (MorphismLoop (MapMorphism lc' g))) bslc)
        
    Call fn args -> do
      let mk i = nameBound ("_a" ++ showPP fn ++ "_" ++ show i)
      (bindss, args') <- unzip <$> zipWithM mk [0 :: Integer ..] args
      pure (doBind (Call fn args') (concat bindss))
    _ -> pure gram'

  where
    doBind = foldl (\body' (v, e) -> Let v e body')
    
    nameBound _n e@(Var _) = pure ([], e)
    nameBound nm e = do
      n <- freshNameSys (typeOf e)
      let n' = n { nameText = Just (Text.pack nm) }
      pure ([(n', e)], Var n')

-- ----------------------------------------------------------------------------------------
-- Eliminate unit typed variables
--
-- Sometimes we name units in Core (e.g. in Choose) which adds noise
-- to the synthesis.
-- 

removeUnitsM :: Module -> Module
removeUnitsM m = m { mGFuns = map removeUnitsGFun (mGFuns m) }

removeUnitsGFun :: Fun Grammar -> Fun Grammar
removeUnitsGFun fu =
  case fDef fu of
    Def g -> fu { fDef = Def (removeUnitsG g) }
    _     -> fu

removeUnitsG :: Grammar -> Grammar
removeUnitsG gram =
  case gebMapChildrenG removeUnitsG removeUnitsE removeUnitsB gram of
    Do n g1 g2 | nameType n == TUnit -> Do_ g1 g2
    Let n _ g  | nameType n == TUnit -> g -- e is pure and can be removed
    g -> g

removeUnitsE :: Expr -> Expr
removeUnitsE expr =
  case mapChildrenE removeUnitsE expr of
    Var n         | nameType n == TUnit -> Ap0 Unit
    PureLet n _ e | nameType n == TUnit -> e
    e -> e

removeUnitsB :: ByteSet -> ByteSet
removeUnitsB bs = 
  case ebMapChildrenB removeUnitsE removeUnitsB bs of
    SetLet n _ b | nameType n == TUnit -> b
    b -> b

-- ----------------------------------------------------------------------------------------
-- Name the result of Match 
--
-- Match will returns its argument if successful, and it is sometimes
-- useful in DDL to use it (for length etc.).
  
nameMatchResultsM :: (Monad m, HasGUID m) => Module -> m Module
nameMatchResultsM m = do
  gfuns' <- mapM nameMatchResultsGFun (mGFuns m)
  pure (m { mGFuns = gfuns' })

nameMatchResultsGFun :: (Monad m, HasGUID m) => Fun Grammar -> m (Fun Grammar)
nameMatchResultsGFun fu =
  case fDef fu of
    Def g -> (\g' -> fu { fDef = Def g' }) <$> nameMatchResultsG True g
    _     -> pure fu

nameMatchResultsG :: (Monad m, HasGUID m) => Bool -> Grammar -> m Grammar
nameMatchResultsG _isTop gram = do
  gram' <- childrenG (nameMatchResultsG False) gram
  nameRHS True gram' -- FIXME: this always introduces a new variable
  -- case gram' of
  --   Do x (Match SemYes (MatchBytes arr)) rhs -> bindMatch x arr <$> nameRHS True rhs
  --   Do_ lhs rhs -> Do_ lhs <$> nameRHS True rhs
  --   _ -> nameRHS isTop gram'

  where
    -- create a new variable if we are in a Match
    nameRHS isTop' gram' =
      case gram' of
        Match SemYes (MatchBytes arr) | isTop' -> do
          x <- freshNameSys (typeOf arr) -- should be TArray (TUInt (TSize 8))
          pure (bindMatch x arr (Pure (Var x)))
        _ -> pure gram'

    bindMatch x arr rhs = Let x arr $ Do_ (Match SemNo (MatchBytes (Var x))) rhs
