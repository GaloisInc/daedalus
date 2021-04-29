
-- This is a hack so that the analysis works when we have constant arguments.

module Talos.Passes (nameConstArgsM) where

import qualified Data.Set as Set

import Daedalus.GUID
import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.Type

nameConstArgsM :: (Monad m, HasGUID m) => Module -> m Module
nameConstArgsM m = do
  gfuns' <- mapM nameConstArgsGFun (mGFuns m)
  pure (m { mGFuns = gfuns' })

nameConstArgsGFun :: (Monad m, HasGUID m) => Fun Grammar -> m (Fun Grammar)
nameConstArgsGFun fu =
  case fDef fu of
    Def g -> (\g' -> fu { fDef = Def g' }) <$> nameConstArgsG g
    _     -> pure fu

nameConstArgsG :: (Monad m, HasGUID m) => Grammar -> m Grammar
nameConstArgsG gram = do
  gram' <- childrenG nameConstArgsG gram
  case gram' of
    Call fn args -> do
      (bindss, args') <- unzip <$> mapM nameArg args
      pure (foldl (\body' (v, e) -> Let v e body') (Call fn args') (concat bindss))
    _ -> pure gram'

  where
    nameArg e
      | not (Set.null (freeVars e)) = pure ([], e)
      | otherwise = do
          n <- freshNameSys (typeOf e)
          pure ([(n, e)], Var n)
