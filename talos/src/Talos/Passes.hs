
-- This is a hack so that the analysis works when we have constant arguments.

module Talos.Passes (nameArgsM, removeUnitsM, nameMatchResultsM, allPassesM ) where

import Daedalus.GUID
import Daedalus.Core
import Daedalus.Core.Type
import Daedalus.Core.Normalize
-- import Daedalus.Core.Inline
import qualified Data.Text as Text
import Daedalus.PP (showPP)
import Control.Monad (zipWithM)

allPassesM :: (Monad m, HasGUID m) => FName -> Module -> m Module
allPassesM _entry m = nameArgsM (removeUnitsM m) >>= nameMatchResultsM  >>= pure . normM
                     -- >>= inlineModule [entry]

-- ----------------------------------------------------------------------------------------
-- Name literal args to functions
--
-- This is required as the analysis needs something to anchor argument slices.
-- FIXME: hack

nameArgsM :: (Monad m, HasGUID m) => Module -> m Module
nameArgsM m = do
  gfuns' <- mapM nameArgsGFun (mGFuns m)
  pure (m { mGFuns = gfuns' })

nameArgsGFun :: (Monad m, HasGUID m) => Fun Grammar -> m (Fun Grammar)
nameArgsGFun fu =
  case fDef fu of
    Def g -> (\g' -> fu { fDef = Def g' }) <$> nameArgsG g
    _     -> pure fu

nameArgsG :: (Monad m, HasGUID m) => Grammar -> m Grammar
nameArgsG gram = do
  gram' <- childrenG nameArgsG gram
  case gram' of
    Call fn args -> do
      (bindss, args') <- unzip <$> zipWithM (nameArg fn) [0 :: Integer ..] args
      pure (foldl (\body' (v, e) -> Let v e body') (Call fn args') (concat bindss))
    _ -> pure gram'

  where
    nameArg _fn _i e@(Var _) = pure ([], e)
    nameArg fn i e = do
      n <- freshNameSys (typeOf e)
      let n' = n { nameText = Just (Text.pack $ "_a" ++ showPP fn ++ "_" ++ show i) }
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
nameMatchResultsG isTop gram = do
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

