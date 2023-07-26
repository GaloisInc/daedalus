
-- Functions which should be in other libraries but aren't

module Talos.Lib where

import qualified SimpleSMT                    as S
import           SimpleSMT                    (SExpr)
import Data.Foldable (toList)

-- -----------------------------------------------------------------------------
-- base

-- short-circuiting
andM :: Monad m => [m Bool] -> m Bool
andM [] = pure True
andM (m : ms) = do
  b <- m
  if b then andM ms else pure False

orM :: Monad m => [m Bool] -> m Bool
orM [] = pure False
orM (m : ms) = do
  b <- m
  if b then pure True else orM ms

findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM f = go . toList
  where
    go [] = pure Nothing
    go (x : xs) = do
      b <- f x
      if b then pure (Just x) else findM f xs

-- -----------------------------------------------------------------------------
-- simple-smt

orMany :: [SExpr] -> SExpr
orMany [x] = x
orMany xs  = S.orMany xs

andMany :: [SExpr] -> SExpr
andMany [x] = x
andMany xs  = S.andMany xs

